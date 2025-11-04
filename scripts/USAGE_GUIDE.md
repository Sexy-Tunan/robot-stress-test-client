# 变量重复赋值检测工具使用指南

## 🎯 快速开始

### 安装要求

- Python 3.6+
- 无需额外依赖（使用标准库）

### 基本用法

```bash
# V2 - 改进版（准确、详细）
python scripts/check_variable_reassignment_v2.py src/

# V2 调试模式
python scripts/check_variable_reassignment_v2.py -v src/problem_file.erl
```





## 💡 实际使用示例

### 场景 1：新项目检查

```bash
# 首次运行，使用V2获得全面检测
python scripts/check_variable_reassignment_v2.py src/ > report.txt 2>&1

# 查看报告
cat report.txt
```

### 场景 2：CI/CD 集成

```bash
# Makefile
.PHONY: check-vars
check-vars:
	@echo "检查变量重复赋值..."
	@python scripts/check_variable_reassignment_v2.py src/

# GitHub Actions
- name: Check Variable Reassignment
  run: python scripts/check_variable_reassignment_v2.py src/
```

### 场景 3：调试特定问题

```bash
# 使用详细模式查看作用域跟踪
python scripts/check_variable_reassignment_v2.py -v src/core/robot.erl

# 输出会显示每一行的处理过程
[DEBUG] L19: A = trans(Type),
[DEBUG]   Scope stack depth: 1, top: ScopeType.FUNCTION
[DEBUG]   Assigned vars: {'A'}
```

### 场景 5：只检查修改的文件

```bash
# 使用 Git 查找修改的文件
git diff --name-only HEAD | grep "\.erl$" | while read file; do
    echo "检查 $file ..."
    python scripts/check_variable_reassignment_v2.py "$file"
done
```

## 🎓 理解检测结果

### 真阳性（True Positive）- 需要修复

```erlang
% 问题代码
func1() ->
    A = 1,
    A = 2,  % ❌ 运行时会 badmatch
    ok.

% 修复方案 1：使用不同的变量名
func1() ->
    A = 1,
    A2 = 2,
    ok.

% 修复方案 2：使用模式匹配（如果是验证）
func1() ->
    A = 1,
    1 = A,  % ✅ 模式匹配验证
    ok.
```

### 假阳性（False Positive）- 可以忽略

```erlang
% 这种情况是合法的，但可能被误报
func2(Type) ->
    case Type of
        1 -> A = 1;
        2 -> A = 2
    end,
    A.  % ✅ 这里使用 A 是合法的
```

**如何判断：**
- 如果代码能正常运行，通常是假阳性
- 如果代码会在运行时报 `badmatch` 错误，是真阳性

### 理解作用域类型

V2 报告中会显示作用域类型：

| 作用域类型 | 含义 | 示例 |
|-----------|------|------|
| `function` | 函数作用域 | `func() -> A = 1, A = 2.` |
| `case_branch` | case分支 | `case X of 1 -> A = 1, A = 2; ... end` |
| `if_branch` | if分支 | `if X > 0 -> A = 1, A = 2; ... end` |
| `receive_branch` | receive分支 | `receive {a} -> A = 1, A = 2 end` |
| `try_block` | try块 | `try A = 1, A = 2 ... end` |
| `catch_block` | catch块 | `catch _:_ -> A = 1, A = 2` |

## ⚙️ 高级配置

### 忽略特定文件

```bash
# 方法 1：使用 grep 过滤
python scripts/check_variable_reassignment_v2.py src/ | grep -v "test_"

# 方法 2：脚本中修改（添加黑名单）
# 在脚本中添加：
IGNORE_PATTERNS = ['*test*.erl', '*_SUITE.erl']
```





## 📚 最佳实践

### 1. 定期运行检测

```bash
# 每周运行一次全量检测
cron: 0 0 * * 0 cd /path/to/project && python scripts/check_variable_reassignment_v2.py src/ > /tmp/check_report.txt
```

### 2. 提交前检查

```bash
# Git pre-commit hook
#!/bin/bash
git diff --cached --name-only | grep "\.erl$" | while read file; do
    python scripts/check_variable_reassignment_v2.py "$file" || exit 1
done
```

