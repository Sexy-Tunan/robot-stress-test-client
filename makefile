PROJECT = chat_room
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
