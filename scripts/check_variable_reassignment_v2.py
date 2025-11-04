#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Erlang 变量重复赋值检测工具 V2
改进版本，支持：
- 嵌套分支作用域隔离
- 匿名函数（fun）独立作用域
- List comprehension 局部作用域
- try-catch-after 结构
- 多行表达式处理
- 更准确的模式匹配识别

使用方法:
    python scripts/check_variable_reassignment_v2.py src/
    python scripts/check_variable_reassignment_v2.py src/boot/test_repeated_assignment.erl
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
    FUNCTION = "function"           # 函数
    CASE_BRANCH = "case_branch"     # case 分支结构
    CASE_CLAUSE = "case_clause"     # case 分支子句
    IF_BRANCH = "if_branch"         # if 分支结构
    IF_CLAUSE = "if_clause"         # if 分支子句
    RECEIVE_BRANCH = "receive_branch"  # receive 分支结构
    RECEIVE_CLAUSE = "receive_clause"  # receive 分支子句
    TRY_BLOCK = "try_block"         # try 块
    CATCH_BLOCK = "catch_block"     # catch 块
    AFTER_BLOCK = "after_block"     # after 块
    FUN = "fun"                     # 匿名函数
    LIST_COMP = "list_comp"         # List comprehension
    BEGIN_BLOCK = "begin_block"     # begin...end 块


@dataclass
class Scope:
    """作用域"""
    scope_type: ScopeType
    start_line: int
    variables: Dict[str, List[Tuple[int, str]]] = field(default_factory=lambda: defaultdict(list))
    # 子作用域是否应该隔离（如不同的 case 分支之间应该隔离）
    isolate_children: bool = False
    # 累积的变量：记录所有子作用域中被赋值的变量（用于变量提升）
    accumulated_vars: Dict[str, Tuple[int, str]] = field(default_factory=dict)


class VariableAssignmentChecker:
    """检测 Erlang 变量重复赋值"""
    
    def __init__(self, verbose: bool = False):
        self.verbose = verbose
        
        # 匹配函数定义（函数定义应该在行首，不能有前导空白或tab）
        # 注意：在 stripped 字符串上匹配，所以这里的 ^ 表示字符串开头
        self.func_pattern = re.compile(r'^([a-z_][a-zA-Z0-9_]*)\s*\(')
        
        # 匹配变量赋值模式（更精确）
        # 匹配 Variable = ... 但排除 ==, =:=, =/=, =<
        self.simple_assign_pattern = re.compile(r'\b([A-Z_][a-zA-Z0-9_]*)\s*=\s*(?!=|:|/|<)')
        
        # 匹配元组/列表模式: {A, B} = ... 或 [H|T] = ...
        self.pattern_match = re.compile(r'^\s*[\{\[]([^\}\]]+)[\}\]]\s*=\s*')
        
        # 分支关键字
        self.branch_keywords = {
            'case': ScopeType.CASE_BRANCH,
            'if': ScopeType.IF_BRANCH,
            'receive': ScopeType.RECEIVE_BRANCH,
            'try': ScopeType.TRY_BLOCK,
        }
        
        self.issues: List[Dict] = []
        
    def log(self, message: str):
        """调试日志"""
        if self.verbose:
            print(f"[DEBUG] {message}", file=sys.stderr)
    
    def is_comment_or_empty(self, line: str) -> bool:
        """判断是否是注释或空行"""
        stripped = line.strip()
        return not stripped or stripped.startswith('%')
    
    def remove_comments_and_strings(self, line: str) -> str:
        """移除注释和字符串内容，防止误匹配
        
        例如: A = "B = C", % X = Y
        应该只检测 A 的赋值
        """
        # 简单处理：先移除字符串（用空格替代）
        # 注意：这是简化版本，完整版需要状态机来处理转义字符
        result = re.sub(r'"(?:[^"\\]|\\.)*"', '""', line)
        result = re.sub(r"'(?:[^'\\]|\\.)*'", "''", result)
        
        # 移除注释
        if '%' in result:
            result = result.split('%')[0]
        
        return result
    
    def extract_variables_from_pattern(self, pattern: str) -> Set[str]:
        """从模式中提取变量名
        
        例如: "{A, {B, C}, [H|T]}" -> {A, B, C, H, T}
        """
        variables = set()
        # 提取所有以大写字母开头的标识符
        for var in re.findall(r'\b([A-Z_][a-zA-Z0-9_]*)\b', pattern):
            if var != '_':  # 排除匿名变量
                variables.add(var)
        return variables
    
    def get_assigned_variables(self, line: str) -> Set[str]:
        """获取一行代码中被赋值的变量
        
        处理多种赋值模式：
        1. Variable = value
        2. {Var1, Var2} = tuple
        3. [H|T] = list
        4. 行内赋值: func(A = value)
        """
        variables = set()
        
        # 移除注释和字符串
        clean_line = self.remove_comments_and_strings(line)
        
        # 跳过空行
        if not clean_line.strip():
            return variables
        
        # 模式匹配赋值: {A, B} = ... 或 [H|T] = ...
        pattern_match = self.pattern_match.match(clean_line)
        if pattern_match:
            variables.update(self.extract_variables_from_pattern(pattern_match.group(1)))
            return variables
        
        # 查找所有赋值（包括行内赋值）
        for match in self.simple_assign_pattern.finditer(clean_line):
            var = match.group(1)
            # 进一步检查：确保不是比较运算符的一部分
            start_pos = match.start()
            end_pos = match.end()
            
            # 检查 = 后面的字符
            if end_pos < len(clean_line):
                next_chars = clean_line[end_pos:end_pos+2]
                if next_chars and next_chars[0] in '=:</>':
                    continue  # 跳过 ==, =:=, =/=, =<
            
            variables.add(var)
        
        return variables
    
    def is_scope_separator(self, line: str) -> Optional[str]:
        """判断是否是作用域分隔符
        
        返回分隔符类型：
        - 'arrow': -> (新分支子句)
        - 'semicolon': ; (分支结束)
        - 'catch': catch (try-catch)
        - 'after': after (try-after 或 receive-after)
        - None: 不是分隔符
        """
        stripped = line.strip()
        
        if '->' in line:
            return 'arrow'
        
        # 检查是否是 catch 或 after 关键字开始
        words = stripped.split()
        if words:
            first_word = words[0]
            if first_word == 'catch':
                return 'catch'
            elif first_word == 'after':
                return 'after'
        
        # 检查行尾的分号（可能在注释前）
        clean_line = self.remove_comments_and_strings(line)
        if clean_line.rstrip().endswith(';'):
            return 'semicolon'
        
        return None
    
    def is_scope_end(self, line: str) -> bool:
        """判断是否是作用域结束（end 关键字）"""
        stripped = line.strip()
        # end 可能单独一行，或者 end, 或 end.
        return bool(re.match(r'end\b', stripped))
    
    def is_fun_start(self, line: str) -> bool:
        """判断是否是匿名函数开始"""
        clean_line = self.remove_comments_and_strings(line)
        return bool(re.search(r'\bfun\s*\(', clean_line))
    
    def is_list_comprehension(self, line: str) -> bool:
        """判断是否是 list comprehension"""
        clean_line = self.remove_comments_and_strings(line)
        # 匹配 [... || ...] 或 [... | ...]（生成器形式）
        return bool(re.search(r'\[.+\|\|.+\]', clean_line))
    
    def check_function_scope_v2(self, lines: List[Tuple[int, str]], 
                                func_name: str, start_line: int) -> List[Dict]:
        """检查函数作用域（改进版本 V2）
        
        使用作用域栈来正确处理嵌套结构和作用域隔离
        """
        issues = []
        
        # 作用域栈：栈顶是当前作用域
        scope_stack: List[Scope] = [
            Scope(ScopeType.FUNCTION, start_line)
        ]
        
        # 用于跟踪分支结构
        branch_clause_count = defaultdict(int)  # 记录每个分支层级的子句数量
        
        for line_num, line in lines:
            stripped = line.strip()
            clean_line = self.remove_comments_and_strings(line)
            
            self.log(f"L{line_num}: {stripped[:50]}")
            self.log(f"  Scope stack depth: {len(scope_stack)}, top: {scope_stack[-1].scope_type}")
            
            # 检测匿名函数开始
            if self.is_fun_start(line):
                self.log(f"  -> Fun start")
                scope_stack.append(Scope(ScopeType.FUN, line_num))
                continue
            
            # 检测 list comprehension（独立作用域）
            if self.is_list_comprehension(line):
                self.log(f"  -> List comprehension (skipping)")
                # 简化处理：list comprehension 通常在一行内，直接跳过
                continue
            
            # 检测分支语句开始
            words = clean_line.strip().split()
            if words and words[0] in self.branch_keywords:
                keyword = words[0]
                scope_type = self.branch_keywords[keyword]
                self.log(f"  -> Branch start: {keyword}")
                scope_stack.append(Scope(scope_type, line_num, isolate_children=True))
                branch_clause_count[len(scope_stack)] = 0
                continue
            
            # 检测作用域分隔符
            separator = self.is_scope_separator(line)
            if separator == 'arrow':
                # -> 开启新的分支子句
                self.log(f"  -> New clause (arrow)")
                
                # 找到最近的分支结构（不是子句）
                # 从栈顶往下找，跳过子句，找到分支结构
                branch_structure_types = {
                    ScopeType.CASE_BRANCH, ScopeType.IF_BRANCH, 
                    ScopeType.RECEIVE_BRANCH
                }
                clause_types = {
                    ScopeType.CASE_CLAUSE, ScopeType.IF_CLAUSE,
                    ScopeType.RECEIVE_CLAUSE
                }
                
                # 检查栈顶是否是子句（说明这是同一分支结构的新子句）
                if len(scope_stack) > 1 and scope_stack[-1].scope_type in clause_types:
                    # Pop 掉上一个子句，并累积变量到父结构
                    clause_scope = scope_stack[-1]
                    self.log(f"    Pop previous clause: {clause_scope.scope_type}")
                    
                    # 将子句中的变量累积到父结构（分支结构）
                    if len(scope_stack) > 1:
                        parent = scope_stack[-2]  # 父结构（CASE_BRANCH等）
                        for var, assignments in clause_scope.variables.items():
                            if var not in parent.accumulated_vars:
                                # 记录第一次赋值的位置
                                parent.accumulated_vars[var] = assignments[0]
                                self.log(f"      Accumulate {var} to parent structure (from arrow)")
                    
                    scope_stack.pop()
                
                # 现在栈顶应该是分支结构，创建新子句
                if len(scope_stack) > 1 and scope_stack[-1].scope_type in branch_structure_types:
                    parent_type = scope_stack[-1].scope_type
                    # 根据父结构类型创建对应的子句类型
                    if parent_type == ScopeType.CASE_BRANCH:
                        clause_type = ScopeType.CASE_CLAUSE
                    elif parent_type == ScopeType.IF_BRANCH:
                        clause_type = ScopeType.IF_CLAUSE
                    elif parent_type == ScopeType.RECEIVE_BRANCH:
                        clause_type = ScopeType.RECEIVE_CLAUSE
                    else:
                        clause_type = parent_type
                    
                    self.log(f"    Push new clause: {clause_type}")
                    scope_stack.append(Scope(clause_type, line_num))
                    branch_clause_count[len(scope_stack)] = branch_clause_count.get(len(scope_stack), 0) + 1
                
            elif separator == 'catch':
                # try...catch
                self.log(f"  -> Catch block")
                # Pop try block, push catch block
                if scope_stack and scope_stack[-1].scope_type == ScopeType.TRY_BLOCK:
                    scope_stack.pop()
                scope_stack.append(Scope(ScopeType.CATCH_BLOCK, line_num))
                
            elif separator == 'after':
                # try...after 或 receive...after
                self.log(f"  -> After block")
                # Pop previous block
                if scope_stack:
                    scope_stack.pop()
                scope_stack.append(Scope(ScopeType.AFTER_BLOCK, line_num))
            
            # 检测作用域结束
            if self.is_scope_end(line):
                self.log(f"  -> Scope end (end keyword)")
                
                # end 会结束一个块结构，可能需要pop多个作用域
                # 1. 如果栈顶是子句，先 pop 子句
                # 2. 然后 pop 分支结构本身
                # 3. 将分支中赋值的变量提升到父作用域（重要！）
                clause_types = {
                    ScopeType.CASE_CLAUSE, ScopeType.IF_CLAUSE,
                    ScopeType.RECEIVE_CLAUSE
                }
                branch_structure_types = {
                    ScopeType.CASE_BRANCH, ScopeType.IF_BRANCH,
                    ScopeType.RECEIVE_BRANCH
                }
                
                if len(scope_stack) > 1:
                    # 如果栈顶是子句，先pop子句
                    if scope_stack[-1].scope_type in clause_types:
                        clause_scope = scope_stack[-1]
                        self.log(f"    Pop clause: {clause_scope.scope_type}")
                        
                        # 将子句中的变量累积到父结构（分支结构）
                        if len(scope_stack) > 1:
                            parent = scope_stack[-2]  # 父结构（CASE_BRANCH等）
                            for var, assignments in clause_scope.variables.items():
                                if var not in parent.accumulated_vars:
                                    # 记录第一次赋值的位置
                                    parent.accumulated_vars[var] = assignments[0]
                                    self.log(f"      Accumulate {var} to parent structure")
                        
                        scope_stack.pop()
                    
                    # 然后pop分支结构/块
                    if len(scope_stack) > 1:
                        closed_scope = scope_stack.pop()
                        self.log(f"    Pop structure: {closed_scope.scope_type}")
                        
                        # ⭐ 关键：如果是分支结构结束，需要将累积的变量提升到父作用域
                        # 因为 case/if/receive 结束后，分支中的变量在外层作用域已经有值了
                        if closed_scope.scope_type in branch_structure_types and len(scope_stack) > 0:
                            parent_scope = scope_stack[-1]
                            branch_vars = set(closed_scope.accumulated_vars.keys())
                            
                            if branch_vars:
                                self.log(f"    Promoting {len(branch_vars)} variables to parent scope: {branch_vars}")
                                
                                # 将这些变量标记为在父作用域中已赋值
                                for var, first_assignment in closed_scope.accumulated_vars.items():
                                    if var not in parent_scope.variables:
                                        parent_scope.variables[var] = [first_assignment]
                                        self.log(f"      Promoted {var} from line {first_assignment[0]}")
                        
                        # 清理分支子句计数
                        depth = len(scope_stack) + 1
                        if depth in branch_clause_count:
                            del branch_clause_count[depth]
            
            # 收集变量赋值
            assigned_vars = self.get_assigned_variables(line)
            
            if assigned_vars:
                self.log(f"  Assigned vars: {assigned_vars}")
                
                # 在当前作用域检查重复赋值
                current_scope = scope_stack[-1]
                
                for var in assigned_vars:
                    if var in current_scope.variables:
                        # 发现重复赋值
                        first_assignment = current_scope.variables[var][0]
                        
                        # 确定错误原因
                        clause_types = {
                            ScopeType.CASE_CLAUSE, ScopeType.IF_CLAUSE,
                            ScopeType.RECEIVE_CLAUSE
                        }
                        
                        if current_scope.scope_type == ScopeType.FUNCTION:
                            reason = '函数内顺序执行中重复赋值'
                        elif current_scope.scope_type in clause_types:
                            reason = '同一分支子句内重复赋值'
                        elif current_scope.scope_type in [ScopeType.CASE_BRANCH, 
                                                          ScopeType.IF_BRANCH, 
                                                          ScopeType.RECEIVE_BRANCH]:
                            reason = '分支结构内重复赋值（非子句）'
                        else:
                            reason = '同一作用域内重复赋值'
                        
                        issues.append({
                            'variable': var,
                            'function': func_name,
                            'first_line': first_assignment[0],
                            'assignments': current_scope.variables[var] + [(line_num, line.strip())],
                            'reason': reason,
                            'scope_type': current_scope.scope_type.value
                        })
                    else:
                        # 首次赋值，记录
                        current_scope.variables[var].append((line_num, line.strip()))
        
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
        
        # 预处理：合并多行表达式（简化版）
        # TODO: 完整实现需要更复杂的解析
        lines = self.merge_multiline_expressions(lines)
        
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
            # 函数定义必须在行首（不能有前导空白），这样可以区分函数定义和函数调用
            func_match = self.func_pattern.match(stripped)
            # 检查原始行是否有前导空白（如果有，说明是函数调用，不是函数定义）
            has_leading_whitespace = line and (line[0] in ' \t')
            
            if func_match and not has_leading_whitespace:
                # 处理上一个函数
                if current_func and func_lines:
                    func_issues = self.check_function_scope_v2(
                        func_lines, current_func, func_start
                    )
                    for issue in func_issues:
                        issue['file'] = filepath
                    issues.extend(func_issues)
                
                # 开始新函数
                current_func = func_match.group(1)
                func_start = line_num
                func_lines = [(line_num, line)]
            elif current_func:
                func_lines.append((line_num, line))
                
                # 函数结束（以 . 结尾）
                if stripped.endswith('.') and not stripped.endswith('..'):
                    func_issues = self.check_function_scope_v2(
                        func_lines, current_func, func_start
                    )
                    for issue in func_issues:
                        issue['file'] = filepath
                    issues.extend(func_issues)
                    current_func = None
                    func_lines = []
        
        # 处理最后一个函数
        if current_func and func_lines:
            func_issues = self.check_function_scope_v2(
                func_lines, current_func, func_start
            )
            for issue in func_issues:
                issue['file'] = filepath
            issues.extend(func_issues)
        
        return issues
    
    def merge_multiline_expressions(self, lines: List[Tuple[int, str]]) -> List[Tuple[int, str]]:
        """合并多行表达式（简化版）
        
        TODO: 完整实现需要括号匹配和更复杂的逻辑
        """
        # 当前版本：不合并，保持原样
        # 未来可以添加括号匹配、逗号连接等逻辑
        return lines
    
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
        # 检测是否支持 Unicode（Windows 控制台可能不支持）
        use_emoji = sys.platform != 'win32'
        
        if not issues:
            print("[OK] 未发现变量重复赋值问题" if not use_emoji else "✅ 未发现变量重复赋值问题")
            return
        
        print(f"\n{'='*80}")
        if use_emoji:
            print(f"🔍 发现 {len(issues)} 个潜在的变量重复赋值问题")
        else:
            print(f"[检测] 发现 {len(issues)} 个潜在的变量重复赋值问题")
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
            
            # 显示错误原因
            if 'reason' in issue:
                if use_emoji:
                    reason_emoji = {
                        '函数内顺序执行中重复赋值': '⚠️',
                        '同一分支子句内重复赋值': '🔀',
                        '同一作用域内重复赋值': '📍'
                    }
                    emoji = reason_emoji.get(issue['reason'], '⚠️')
                    print(f"  {emoji} 原因: {issue['reason']}")
                else:
                    print(f"  [原因] {issue['reason']}")
            
            if use_emoji:
                print(f"  🔄 赋值位置 ({len(issue['assignments'])} 次):\n")
            else:
                print(f"  [赋值位置] ({len(issue['assignments'])} 次):\n")
            
            for line_num, line_content in issue['assignments']:
                # 截断过长的行
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
            print(f"⚠️  总计: {len(issues)} 个问题需要修复")
        else:
            print(f"[警告] 总计: {len(issues)} 个问题需要修复")
        print(f"{'='*80}\n")


def main():
    """主函数"""
    verbose = '--verbose' in sys.argv or '-v' in sys.argv
    
    args = [arg for arg in sys.argv[1:] if arg not in ['--verbose', '-v']]
    
    if len(args) < 1:
        print("用法: python check_variable_reassignment_v2.py [选项] <文件或目录路径>")
        print("\n选项:")
        print("  -v, --verbose    显示详细调试信息")
        print("\n示例:")
        print("  python scripts/check_variable_reassignment_v2.py src/")
        print("  python scripts/check_variable_reassignment_v2.py -v src/boot/test_repeated_assignment.erl")
        sys.exit(1)
    
    checker = VariableAssignmentChecker(verbose=verbose)
    
    all_issues = []
    for path in args:
        issues = checker.check_path(path)
        all_issues.extend(issues)
    
    checker.print_report(all_issues)
    
    # 如果发现问题，返回非零退出码
    if all_issues:
        sys.exit(1)
    else:
        sys.exit(0)


if __name__ == '__main__':
    main()

