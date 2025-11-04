PROJECT = robot_client
REBAR = rebar3
EBIN = _build/default/lib/$(PROJECT)/ebin

# 默认目标
all: compile

# 清理 + 编译
compile:
	@$(REBAR) clean
	@$(REBAR) compile

# 启动 erl shell 并自动加载依赖与应用
run:
	@$(REBAR) compile
	@erl -pa _build/default/lib/*/ebin -eval " application:ensure_all_started($(PROJECT))."

# 只启动 shell，不启动 app
shell:
	@$(REBAR) compile
	@erl -pa _build/default/lib/*/ebin

# 一键重启（清理 + 编译 + 启动）
restart:
	@$(REBAR) clean
	@$(REBAR) compile
	@erl -pa _build/default/lib/*/ebin -eval "application:ensure_all_started(jsx), application:ensure_all_started($(PROJECT)), application:start($(PROJECT))."


# 清理
clean:
	@$(REBAR) clean

# ==================== 代码质量检查 ====================

# 检查变量重复赋值
check-vars:
	@echo "🔍 检查变量重复赋值..."
	@python scripts/check_variable_reassignment_v2.py src/

# 运行 dialyzer 类型检查
check-dialyzer:
	@echo "🔍 运行 Dialyzer 类型检查..."
	@$(REBAR) dialyzer

# 运行单元测试
test:
	@echo "🧪 运行单元测试..."
	@$(REBAR) eunit

# 完整的代码质量检查
check-all: compile check-vars check-dialyzer test
	@echo "✅ 所有检查完成！"

# 快速检查（不包括 dialyzer，因为它比较慢）
check-quick: compile check-vars test
	@echo "✅ 快速检查完成！"