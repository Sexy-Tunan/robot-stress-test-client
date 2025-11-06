# Erlang变量重复赋值检测工具 - 版本历史
## V13 (最新版本)

### 核心改进

支持跨行赋值检测：变量名和等号可以在不同行。

### 问题描述

**场景：**

```erlang
test3() ->
    Score = 2,       % 第33行：第一次赋值
    Score            % 第34行：变量名
    = getNextScore(a),  % 第35行：等号和值在下一行
    ok.
```

在Erlang中，允许将赋值语句换行，但V12版本无法检测这种跨行的重复赋值。

### 解决方案

#### 1. 添加跨行检测正则表达式

```python
# 检测以等号开头的行（跨行赋值的第二行）
self.line_start_assign = re.compile(r'^\s*=\s*(?!=|:|/|<)')

# 检测行末的变量（可能在下一行继续赋值）
self.line_end_variable = re.compile(r'(?<!\$)\b([A-Z_][a-zA-Z0-9_]*)\s*$')
```

#### 2. 改进变量赋值检测

在`get_assigned_variables`方法中添加`prev_line`参数：

```python
def get_assigned_variables(self, line: str, prev_line: Optional[str] = None) -> Set[str]:
    """获取一行代码中被赋值的变量
    
    Args:
        line: 当前行
        prev_line: 上一行（用于检测跨行赋值）
    """
    # 如果当前行以 = 开头，检查上一行末尾是否有变量
    if prev_line and self.line_start_assign.match(clean_line):
        clean_prev = self.remove_comments_and_strings(prev_line)
        match = self.line_end_variable.search(clean_prev)
        if match:
            var = match.group(1)
            if var != '_' and not var.startswith('_'):
                variables.add(var)
                return variables
    # ... 其他检测逻辑
```

#### 3. 在主循环中记录上一行

```python
# 记录上一行内容（用于跨行赋值检测）
prev_line_content = None

for line_num, line in lines:
    # ... 处理逻辑
    
    # 传入上一行内容
    assigned_vars = self.get_assigned_variables(line, prev_line_content)
    
    # 更新上一行内容
    prev_line_content = line
```



### 跨行赋值的识别规则

1. **当前行以`=`开头**（排除`==`、`=:=`等）
2. **上一行末尾有变量**（大写字母或`_`开头）
3. **变量不是匿名变量**（不是`_`或`_`开头）
4. **变量前面不是`$`**（排除字符字面量）

### 保持的功能

- V12的所有功能：字符字面量正确处理
- V11的所有功能：无emoji输出
- V10的所有功能：匿名函数作用域、多子句fun支持
- 正确区分case/if/receive分支的独立性

### 测试结果

✅ 跨行赋值能被正确检测  
✅ 字符字面量（`$N`等）不误判  
✅ 真实重复赋值仍能正确检测  
✅ 保持V12的所有功能和修复

## V12

### 核心改进
修复字符字面量误判：正确识别`$N`、`$A`等不是变量赋值。

### 问题描述
**场景：**
```erlang
test2(Socket, TimeOut) ->
    {ok,<<$N,_MyFlags:64,...>>} = gen_tcp:recv(Socket,0,TimeOut),
    {ok,<<$N,_OtherFlags:64,...>>} = gen_tcp:recv(Socket,0,TimeOut).
```

在Erlang中，`$N`是**字符字面量**（character literal），表示字符'N'的ASCII码值（78），不是变量赋值。

但V11版本误将`$N`中的`N`识别为变量，导致误报。

### 解决方案

1. **改进正则表达式**：添加负向后顾断言
   ```python
   # 旧版本（V11）
   r'\b([A-Z_][a-zA-Z0-9_]*)\s*=\s*(?!=|:|/|<)'
   
   # 新版本（V12）
   r'(?<!\$)\b([A-Z_][a-zA-Z0-9_]*)\s*=\s*(?!=|:|/|<)'
   ```

   `(?<!\$)`表示变量名前面不能是`$`符号

2. **增强模式匹配处理**：在`extract_variables_from_pattern`中先移除字符字面量
   ```python
   pattern_no_char_literals = re.sub(r'\$[A-Za-z0-9_]', '', pattern)
   ```

### Erlang变量规则说明

根据Erlang语法：
- **变量**：必须以大写字母或`_`开头，由字母、数字、下划线组成
   - 示例：`X`、`Name`、`_Temp`、`Result1`
- **字符字面量**：`$`后跟单个字符，表示该字符的ASCII码值
   - 示例：`$A` = 65、`$N` = 78、`$0` = 48

### 保持的功能
- V11的所有功能：无emoji输出
- V10的所有功能：匿名函数作用域、多子句fun支持
- 正确区分case/if/receive分支的独立性

### 测试结果
✅ 字符字面量（`$N`等）不再误判  
✅ 真实重复赋值仍能正确检测  
✅ 保持V11的所有功能和修复


## V11

### 核心改进
完全移除emoji特殊符号，使用纯文本标记，彻底解决Windows平台显示问题。

### 改进内容
1. **输出格式**：所有emoji符号改为纯文本标记
   - `✅` → `[OK]`
   - `🔍` → `[检测]`
   - `📁` → `[文件]`
   - `📝` → `[函数]`
   - `🔤` → `[变量]`
   - `📍` → `[作用域]`
   - `⚠️` → `[原因]` / `[警告]`
   - `🔄` → `[赋值位置]`
   - `💡` → `[建议]`

2. **调试日志**：verbose模式的日志也使用纯文本
   - `⚠️  Duplicate` → `[WARNING] Duplicate`

3. **代码简化**：完全移除`use_emoji`相关的条件分支

### 保持的功能
- V10的所有功能：匿名函数作用域、多子句fun支持
- 正确区分case/if/receive分支的独立性
- 准确检测同一作用域内的真实重复赋值

### 测试结果
✅ 输出完全不含emoji特殊符号  
✅ 调试日志使用纯文本标记  
✅ 保持V10的所有功能和修复  

## V10

### 核心改进
1. 修复了V9版本中匿名函数作用域的处理问题
2. 正确支持多子句匿名函数
3. 移除emoji输出，避免Windows平台显示问题

### 问题1：单行fun关键字未被识别
**场景：**
```erlang
GainsList = lists:foldl(fun(CfgID, Acc) ->  % 单行，fun后跟括号
    Round = ...,  % 匿名函数内
end, [], ...),
Round = ...,  % 函数主体 - 应该不冲突
```

**问题：** V9版本将匿名函数当作分支结构处理，导致误报。

**解决：**
- 添加`StructureContext`和`structure_stack`跟踪所有结构
- 改进`end`关键字匹配逻辑，确保匹配正确的结构
- 匿名函数创建独立的嵌套作用域（`FUN_BODY`）

### 问题2：多子句匿名函数
**场景：**
```erlang
Fun = fun
    (1) -> X = 1;     % 子句1
    (2) -> X = 2;     % 子句2 - 应该不与子句1冲突
    (3) -> X = 3      % 子句3 - 最后一个子句无分号
end,
X = Fun(1),  % 函数主体 - 应该不与fun内冲突
```

**问题：** 
1. 单独一行的`fun`关键字未被识别（正则要求`fun(`）
2. 最后一个子句没有分号，导致与前一个子句在同一作用域

**解决：**
1. 改进`is_fun_start`，支持`fun`单独一行的情况：`r'\bfun\s*(\(|$)'`
2. 检测新fun子句时，判断行是否以`(`开头，即使前面没有`;`也能识别

### 问题3：输出格式
移除emoji符号，使用纯文本标记（`[OK]`、`[检测]`、`[文件]`等），避免Windows平台乱码。

### 技术细节
- **structure_stack**: 精确跟踪所有打开的结构，确保`end`正确匹配
- **is_new_fun_clause**: 通过检查行首是否为`(`来识别新的fun子句
- **use_emoji = False**: 固定不使用emoji输出

### 保持的功能
- 正确区分case/if/receive不同分支的独立性
- 准确检测同一作用域内的真实重复赋值
- 支持嵌套的分支和匿名函数结构
- 支持begin...end块

### 测试结果
✅ 修复单行fun的误报（mod_option_lottery.erl）
✅ 修复多子句fun的误报（用户报告的test.erl）
✅ 输出格式不再使用emoji
✅ 保持对真实重复赋值的检测能力
✅ 通过了多个模块的测试（common/map/gateway/welfare）

## V9

### 核心改进
- 正确处理嵌套的case/if/receive分支
- 修复了深度追踪问题
- 只报告同一顺序执行路径中的重复赋值

### 已知问题
❌ 匿名函数作用域处理不正确，导致误报（已在V10修复）

## 使用方法

```bash
# 检查单个文件
python check_variable_reassignment_v11.py src/test/test_file.erl

# 检查整个目录
python check_variable_reassignment_v11.py src/

# 显示详细调试信息
python check_variable_reassignment_v11.py -v src/test/test_file.erl
```

## 建议
- **推荐使用V11版本**（`check_variable_reassignment_v11.py`）进行检测
- V11完全移除emoji，输出格式友好，适合Windows平台
- V10版本已尝试移除emoji但不彻底
- V9及之前的版本存在匿名函数作用域误判问题，不建议使用

