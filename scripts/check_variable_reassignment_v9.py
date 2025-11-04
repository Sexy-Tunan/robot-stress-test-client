#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Erlang 变量重复赋值检测工具 V6
核心改进：
- 正确区分不同case/if/receive分支（每个分支独立，不应相互比较）
- 修复了v5中对分支独立性判断的问题
- 只报告在同一顺序执行路径中的真实重复赋值

使用方法:
    python check_variable_reassignment_v6.py src/
    python check_variable_reassignment_v6.py src/test/test_file.erl
"""

import re
import os
import sys
from pathlib import Path
from typing import List, Dict, Tuple, Set, Optional
from collections import defaultdict
from dataclasses import dataclass, field
from enum import Enum


class ScopeType(Enum):
    """作用域类型"""
    FUNCTION = "function"           # 函数主体
    CASE_CLAUSE = "case_clause"     # case的一个分支子句
    IF_CLAUSE = "if_clause"         # if的一个分支子句
    RECEIVE_CLAUSE = "receive_clause"  # receive的一个分支子句
    FUN_CLAUSE = "fun_clause"       # 匿名函数的一个子句
    BLOCK = "block"                 # begin...end块


@dataclass
class Scope:
    """作用域"""
    scope_type: ScopeType
    start_line: int
    # 记录每个变量的赋值列表
    variables: Dict[str, List[Tuple[int, str]]] = field(default_factory=lambda: defaultdict(list))
    # 记录这个作用域属于哪个分支深度（用于区分嵌套分支）
    branch_depth: int = 0


@dataclass
class BranchContext:
    """分支上下文"""
    branch_type: str  # 'case', 'if', 'receive', 'try', 'begin'
    start_line: int
    

class VariableAssignmentChecker:
    """检测 Erlang 变量重复赋值"""
    
    def __init__(self, verbose: bool = False):
        self.verbose = verbose
        self.func_pattern = re.compile(r'^([a-z_][a-zA-Z0-9_]*)\s*\(')
        self.simple_assign_pattern = re.compile(r'\b([A-Z_][a-zA-Z0-9_]*)\s*=\s*(?!=|:|/|<)')
        # 模式匹配赋值：{...} = 或 [...] =，但等号后不能是 =, :, /, < (排除 ==, =:=, =<, =/= 等)
        self.pattern_match = re.compile(r'^\s*[\{\[]([^\}\]]+)[\}\]]\s*=\s*(?!=|:|/|<)')
        self.issues: List[Dict] = []
        
    def log(self, message: str):
        """调试日志"""
        if self.verbose:
            print(f"[DEBUG] {message}", file=sys.stderr)
    
    def remove_comments_and_strings(self, line: str) -> str:
        """移除注释和字符串内容"""
        result = re.sub(r'"(?:[^"\\]|\\.)*"', '""', line)
        result = re.sub(r"'(?:[^'\\]|\\.)*'", "''", result)
        if '%' in result:
            result = result.split('%')[0]
        return result
    
    def extract_variables_from_pattern(self, pattern: str) -> Set[str]:
        """从模式中提取变量名，过滤匿名变量"""
        variables = set()
        for var in re.findall(r'\b([A-Z_][a-zA-Z0-9_]*)\b', pattern):
            # 过滤匿名变量 _ 和下划线开头的变量
            if var != '_' and not var.startswith('_'):
                variables.add(var)
        return variables
    
    def is_list_comprehension(self, line: str) -> bool:
        """判断是否是列表推导式"""
        clean_line = self.remove_comments_and_strings(line)
        return bool(re.search(r'\[.+(\|\||<-).+\]', clean_line))
    
    def get_assigned_variables(self, line: str) -> Set[str]:
        """获取一行代码中被赋值的变量，过滤匿名变量"""
        variables = set()
        clean_line = self.remove_comments_and_strings(line)
        
        if not clean_line.strip():
            return variables
        
        # 跳过列表推导式
        if self.is_list_comprehension(line):
            return variables
        
        # 模式匹配赋值
        pattern_match = self.pattern_match.match(clean_line)
        if pattern_match:
            variables.update(self.extract_variables_from_pattern(pattern_match.group(1)))
            return variables
        
        # 简单赋值
        for match in self.simple_assign_pattern.finditer(clean_line):
            var = match.group(1)
            # 过滤匿名变量和下划线开头的变量
            if var == '_' or var.startswith('_'):
                continue
                
            end_pos = match.end()
            if end_pos < len(clean_line):
                next_chars = clean_line[end_pos:end_pos+2]
                if next_chars and next_chars[0] in '=:</>':
                    continue
            variables.add(var)
        
        return variables
    
    def has_branch_keyword(self, line: str) -> Tuple[bool, Optional[str]]:
        """检测行中是否包含分支关键字"""
        clean_line = self.remove_comments_and_strings(line)
        
        if re.search(r'\bcase\b', clean_line) and re.search(r'\bof\b', clean_line):
            return (True, 'case')
        elif re.search(r'\bif\b(?!\w)', clean_line):
            return (True, 'if')
        elif re.search(r'\breceive\b', clean_line):
            return (True, 'receive')
        elif re.search(r'\btry\b', clean_line):
            return (True, 'try')
        elif re.search(r'\bbegin\b', clean_line):
            return (True, 'begin')
        
        return (False, None)
    
    def has_clause_arrow(self, line: str) -> bool:
        """检测是否包含子句箭头 ->（不是 =>）"""
        clean_line = self.remove_comments_and_strings(line)
        return bool(re.search(r'->', clean_line) and '=>' not in clean_line)
    
    def has_clause_separator(self, line: str) -> bool:
        """检测是否包含子句分隔符 ;
        
        关键：在Erlang中，分号用于分隔case/if/receive的不同分支
        分号可以在行尾或行中间
        """
        clean_line = self.remove_comments_and_strings(line).strip()
        if ';' not in clean_line:
            return False
        
        # 分号可以在行尾（下一行开始新分支）或行中间
        # 但要排除一些特殊情况：
        # 1. [H|T] 这种列表语法中的 |
        # 2. 注释中的分号
        
        # 简单方法：如果有分号，就认为可能是分支分隔符
        # 但要确保不是在某些特殊上下文中
        
        # 检查是否在代码行中有分号（不仅仅是注释）
        if clean_line.endswith(';'):
            # 行尾分号，很可能是分支分隔
            # 但要排除空语句或特殊情况
            before_semicolon = clean_line[:-1].strip()
            if before_semicolon:  # 分号前有实际内容
                return True
        
        # 分号在行中间
        parts = clean_line.split(';')
        for i, part in enumerate(parts[:-1]):
            next_part = parts[i+1].strip()
            # 如果分号后还有代码，这是一个分支分隔符
            if next_part and not next_part.startswith('%'):
                return True
        
        return False
    
    def count_end_keywords(self, line: str) -> int:
        """计算 end 关键字数量"""
        clean_line = self.remove_comments_and_strings(line)
        return len(re.findall(r'\bend\b', clean_line))
    
    def is_fun_start(self, line: str) -> bool:
        """判断是否是匿名函数开始"""
        clean_line = self.remove_comments_and_strings(line)
        return bool(re.search(r'\bfun\s*\(', clean_line))
    
    def is_fun_clause_start(self, line: str) -> bool:
        """判断是否是匿名函数的子句开始（参数模式）
        
        匿名函数可以有多个子句，类似：
        fun(Pattern1) -> ... ;
           (Pattern2) -> ...
        end
        """
        clean_line = self.remove_comments_and_strings(line).strip()
        # 检查是否以 ( 开头（fun的子句模式），且后面有 ->
        # 这种情况通常出现在fun的第二个及以后的子句
        return bool(re.match(r'^\s*\(.*\)\s*(->\s*|,)', clean_line))
    
    def check_function_v6(self, lines: List[Tuple[int, str]], 
                          func_name: str, start_line: int) -> List[Dict]:
        """检查函数（V6版本）
        
        核心改进：
        1. 正确处理case/if/receive的分支独立性
        2. 每个分支（用 -> ... ; 分隔）都是独立的作用域
        3. 不同分支之间不应该比较变量赋值
        4. 只报告同一顺序执行路径中的真实重复赋值
        """
        issues = []
        
        # 作用域栈：始终保持函数作用域在栈底
        scope_stack: List[Scope] = [Scope(ScopeType.FUNCTION, start_line)]
        
        # 分支栈：跟踪嵌套的分支结构
        branch_stack: List[BranchContext] = []
        
        # 是否在等待分支子句开始（遇到了case/if但还没遇到->）
        waiting_for_clause = False
        
        for line_num, line in lines:
            stripped = line.strip()
            clean_line = self.remove_comments_and_strings(line)
            
            self.log(f"L{line_num}: {stripped[:60]}")
            self.log(f"  Scopes: {[(s.scope_type.value, s.start_line) for s in scope_stack]}")
            self.log(f"  Branches: {[(b.branch_type, b.start_line) for b in branch_stack]}")
            
            # 跳过空行和注释
            if not stripped or stripped.startswith('%'):
                continue
            
            # 1. 检测匿名函数开始
            # fun像case一样，可以有多个子句，每个子句是独立的作用域
            if self.is_fun_start(line):
                self.log(f"  -> Fun start (treated as branch structure)")
                branch_stack.append(BranchContext('fun', line_num))
                waiting_for_clause = True  # 等待第一个子句的 ->
                continue
            
            # 2. 检测分支关键字（case/if/receive/try/begin）
            has_branch, branch_type = self.has_branch_keyword(line)
            if has_branch:
                branch_stack.append(BranchContext(branch_type, line_num))
                waiting_for_clause = True
                self.log(f"  -> Branch keyword: {branch_type}")
                continue
            
            # 3. 检测 end 关键字
            # 关键：end 只关闭对应的branch结构，不主动关闭外层的clause
            end_count = self.count_end_keywords(line)
            if end_count > 0:
                self.log(f"  -> Found {end_count} end keyword(s)")
                for _ in range(end_count):
                    # 检查并关闭最内层的分支结构
                    if branch_stack:
                        # 获取要关闭的分支类型
                        branch_to_close = branch_stack[-1]
                        current_depth = len(branch_stack)
                        
                        # 检查是否需要先关闭子句
                        # 只有当栈顶的clause属于要关闭的分支，且深度匹配时，才pop clause
                        if (len(scope_stack) > 1):
                            top_scope = scope_stack[-1]
                            # 对于匿名函数，只关闭 FUN_CLAUSE
                            if (branch_to_close.branch_type == 'fun' and 
                                top_scope.scope_type == ScopeType.FUN_CLAUSE and
                                top_scope.branch_depth == current_depth):
                                old_scope = scope_stack.pop()
                                self.log(f"    Pop fun clause scope: {old_scope.scope_type.value}")
                            # 对于 case/if/receive，只有当深度匹配时才关闭
                            elif (branch_to_close.branch_type in ['case', 'if', 'receive', 'try'] and
                                  top_scope.scope_type in {
                                      ScopeType.CASE_CLAUSE, ScopeType.IF_CLAUSE, 
                                      ScopeType.RECEIVE_CLAUSE
                                  } and
                                  top_scope.branch_depth == current_depth):
                                old_scope = scope_stack.pop()
                                self.log(f"    Pop clause scope: {old_scope.scope_type.value}")
                            # 对于 begin，只有当深度匹配时才关闭
                            elif (branch_to_close.branch_type == 'begin' and
                                  top_scope.scope_type == ScopeType.BLOCK and
                                  top_scope.branch_depth == current_depth):
                                old_scope = scope_stack.pop()
                                self.log(f"    Pop block scope: {old_scope.scope_type.value}")
                        
                        # 关闭分支
                        closed_branch = branch_stack.pop()
                        waiting_for_clause = False
                        self.log(f"    Close branch: {closed_branch.branch_type}")
            
            # 4. 检测子句分隔符 ; 
            # 关键：分号表示当前分支结束，下一个分支开始
            # 只关闭匹配当前分支深度的clause，不影响外层clause
            if self.has_clause_separator(line) and branch_stack:
                current_depth = len(branch_stack)
                if (len(scope_stack) > 1 and 
                    scope_stack[-1].scope_type in {
                        ScopeType.CASE_CLAUSE, ScopeType.IF_CLAUSE, 
                        ScopeType.RECEIVE_CLAUSE, ScopeType.FUN_CLAUSE
                    } and
                    scope_stack[-1].branch_depth == current_depth):
                    # 只有当clause的深度匹配当前分支深度时才关闭
                    old_scope = scope_stack.pop()
                    waiting_for_clause = True  # 等待下一个分支的 ->
                    self.log(f"  -> Clause separator (;) at depth {current_depth}, end current branch")
                    self.log(f"    Popped scope: {old_scope.scope_type.value}, waiting for next clause")
            
            # 5. 检测子句开始（->）
            # 这是一个新的独立分支的开始
            if self.has_clause_arrow(line) and branch_stack:
                current_depth = len(branch_stack)
                self.log(f"  -> Clause arrow (->), start new branch at depth {current_depth}")
                
                # 只有当栈顶clause的深度等于当前分支深度时，才关闭它（同一层的新分支）
                # 如果深度不同，说明栈顶clause属于外层，不应该关闭
                if (len(scope_stack) > 1 and 
                    scope_stack[-1].scope_type in {
                        ScopeType.CASE_CLAUSE, ScopeType.IF_CLAUSE, 
                        ScopeType.RECEIVE_CLAUSE, ScopeType.FUN_CLAUSE
                    } and
                    scope_stack[-1].branch_depth == current_depth):
                    old_scope = scope_stack.pop()
                    self.log(f"    Popped same-level clause: {old_scope.scope_type.value}")
                
                # 创建新的子句作用域，记录当前分支深度
                current_branch = branch_stack[-1]
                
                if current_branch.branch_type == 'case':
                    scope_stack.append(Scope(ScopeType.CASE_CLAUSE, line_num, branch_depth=current_depth))
                elif current_branch.branch_type in ['if', 'try']:
                    scope_stack.append(Scope(ScopeType.IF_CLAUSE, line_num, branch_depth=current_depth))
                elif current_branch.branch_type == 'receive':
                    scope_stack.append(Scope(ScopeType.RECEIVE_CLAUSE, line_num, branch_depth=current_depth))
                elif current_branch.branch_type == 'begin':
                    scope_stack.append(Scope(ScopeType.BLOCK, line_num, branch_depth=current_depth))
                elif current_branch.branch_type == 'fun':
                    scope_stack.append(Scope(ScopeType.FUN_CLAUSE, line_num, branch_depth=current_depth))
                
                waiting_for_clause = False
                self.log(f"    Created new clause scope at depth {current_depth}")
            
            # 6. 收集变量赋值（如果不在等待子句状态）
            if not waiting_for_clause:
                assigned_vars = self.get_assigned_variables(line)
                
                if assigned_vars:
                    self.log(f"  Assigned vars: {assigned_vars}")
                    current_scope = scope_stack[-1]
                    
                    for var in assigned_vars:
                        if var in current_scope.variables:
                            # 发现重复赋值
                            first_assignment = current_scope.variables[var][0]
                            
                            # 确定原因和是否报告
                            should_report = True
                            
                            if current_scope.scope_type == ScopeType.FUNCTION:
                                reason = '函数内顺序执行中重复赋值'
                            elif current_scope.scope_type in {
                                ScopeType.CASE_CLAUSE, ScopeType.IF_CLAUSE, 
                                ScopeType.RECEIVE_CLAUSE, ScopeType.FUN_CLAUSE
                            }:
                                reason = '同一分支子句内重复赋值'
                            elif current_scope.scope_type == ScopeType.BLOCK:
                                reason = 'begin块内重复赋值'
                            else:
                                reason = '同一作用域内重复赋值'
                            
                            if should_report:
                                self.log(f"    ⚠️  Duplicate: {var} ({reason})")
                                
                                issues.append({
                                    'variable': var,
                                    'function': func_name,
                                    'first_line': first_assignment[0],
                                    'assignments': current_scope.variables[var] + [(line_num, line.strip())],
                                    'reason': reason,
                                    'scope_type': current_scope.scope_type.value
                                })
                        else:
                            # 首次赋值
                            current_scope.variables[var].append((line_num, line.strip()))
                            self.log(f"    First assignment: {var}")
        
        return issues
    
    def analyze_file(self, filepath: str) -> List[Dict]:
        """分析单个文件"""
        issues = []
        
        try:
            with open(filepath, 'r', encoding='utf-8') as f:
                lines = [(i + 1, line) for i, line in enumerate(f.readlines())]
        except Exception as e:
            print(f"[ERROR] 无法读取文件 {filepath}: {e}", file=sys.stderr)
            return issues
        
        # 函数分割
        current_func = None
        func_lines = []
        func_start = 0
        
        for line_num, line in lines:
            stripped = line.strip()
            
            # 跳过模块属性
            if self.is_module_directive(stripped):
                continue
            
            # 检测函数定义
            func_match = self.func_pattern.match(stripped)
            has_leading_whitespace = line and (line[0] in ' \t')
            
            if func_match and not has_leading_whitespace:
                # 处理上一个函数
                if current_func and func_lines:
                    func_issues = self.check_function_v6(func_lines, current_func, func_start)
                    for issue in func_issues:
                        issue['file'] = filepath
                    issues.extend(func_issues)
                
                # 开始新函数
                current_func = func_match.group(1)
                func_start = line_num
                func_lines = [(line_num, line)]
            elif current_func:
                func_lines.append((line_num, line))
                
                # 函数结束
                if stripped.endswith('.') and not stripped.endswith('..'):
                    func_issues = self.check_function_v6(func_lines, current_func, func_start)
                    for issue in func_issues:
                        issue['file'] = filepath
                    issues.extend(func_issues)
                    current_func = None
                    func_lines = []
        
        # 处理最后一个函数
        if current_func and func_lines:
            func_issues = self.check_function_v6(func_lines, current_func, func_start)
            for issue in func_issues:
                issue['file'] = filepath
            issues.extend(func_issues)
        
        return issues
    
    def is_module_directive(self, line: str) -> bool:
        """判断是否是模块指令"""
        return bool(
            not line or 
            line.startswith('%') or 
            line.startswith('-module') or
            line.startswith('-author') or
            line.startswith('-export') or
            line.startswith('-import') or
            line.startswith('-include') or
            line.startswith('-define') or
            line.startswith('-record') or
            line.startswith('-type') or
            line.startswith('-spec') or
            line.startswith('-ifdef') or
            line.startswith('-ifndef') or
            line.startswith('-endif')
        )
    
    def check_path(self, path: str) -> List[Dict]:
        """检查路径（文件或目录）"""
        all_issues = []
        
        path_obj = Path(path)
        
        if path_obj.is_file():
            if path_obj.suffix == '.erl':
                all_issues.extend(self.analyze_file(str(path_obj)))
        elif path_obj.is_dir():
            for erl_file in path_obj.rglob('*.erl'):
                all_issues.extend(self.analyze_file(str(erl_file)))
        else:
            print(f"[ERROR] 路径不存在: {path}", file=sys.stderr)
        
        return all_issues
    
    def print_report(self, issues: List[Dict]):
        """打印检测报告"""
        use_emoji = sys.platform != 'win32'
        
        if not issues:
            print("✅ 未发现变量重复赋值问题" if use_emoji else "[OK] 未发现变量重复赋值问题")
            return
        
        print(f"\n{'='*80}")
        if use_emoji:
            print(f"🔍 发现 {len(issues)} 个真实的变量重复赋值问题")
        else:
            print(f"[检测] 发现 {len(issues)} 个真实的变量重复赋值问题")
        print(f"{'='*80}\n")
        
        for i, issue in enumerate(issues, 1):
            print(f"问题 #{i}:")
            if use_emoji:
                print(f"  📁 文件: {issue['file']}")
                print(f"  📝 函数: {issue['function']}()")
                print(f"  🔤 变量: {issue['variable']}")
                print(f"  📍 作用域: {issue.get('scope_type', 'unknown')}")
            else:
                print(f"  [文件] {issue['file']}")
                print(f"  [函数] {issue['function']}()")
                print(f"  [变量] {issue['variable']}")
                print(f"  [作用域] {issue.get('scope_type', 'unknown')}")
            
            if 'reason' in issue:
                if use_emoji:
                    print(f"  ⚠️  原因: {issue['reason']}")
                else:
                    print(f"  [原因] {issue['reason']}")
            
            if use_emoji:
                print(f"  🔄 赋值位置 ({len(issue['assignments'])} 次):\n")
            else:
                print(f"  [赋值位置] ({len(issue['assignments'])} 次):\n")
            
            for line_num, line_content in issue['assignments']:
                if len(line_content) > 70:
                    line_content = line_content[:67] + '...'
                print(f"      第 {line_num} 行: {line_content}")
            
            if use_emoji:
                print(f"\n  💡 建议: 使用不同的变量名，如 {issue['variable']}1, {issue['variable']}2")
            else:
                print(f"\n  [建议] 使用不同的变量名，如 {issue['variable']}1, {issue['variable']}2")
            print(f"  {'─'*76}\n")
        
        print(f"{'='*80}")
        if use_emoji:
            print(f"⚠️  总计: {len(issues)} 个真实问题需要修复")
        else:
            print(f"[警告] 总计: {len(issues)} 个真实问题需要修复")
        print(f"{'='*80}\n")


def main():
    """主函数"""
    verbose = '--verbose' in sys.argv or '-v' in sys.argv
    
    args = [arg for arg in sys.argv[1:] if arg not in ['--verbose', '-v']]
    
    if len(args) < 1:
        print("用法: python check_variable_reassignment_v6.py [选项] <文件或目录路径>")
        print("\n选项:")
        print("  -v, --verbose    显示详细调试信息")
        print("\nV6版本改进:")
        print("  - 正确区分不同case/if/receive分支的独立性")
        print("  - 修复了v5对分支作用域的误判")
        print("  - 只报告真实的顺序执行中的重复赋值")
        print("\n示例:")
        print("  python check_variable_reassignment_v6.py src/")
        print("  python check_variable_reassignment_v6.py -v src/test/test_file.erl")
        sys.exit(1)
    
    checker = VariableAssignmentChecker(verbose=verbose)
    
    all_issues = []
    for path in args:
        issues = checker.check_path(path)
        all_issues.extend(issues)
    
    checker.print_report(all_issues)
    
    if all_issues:
        sys.exit(1)
    else:
        sys.exit(0)


if __name__ == '__main__':
    main()

