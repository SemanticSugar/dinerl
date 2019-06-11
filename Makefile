REBAR?=rebar3

all: compile test edoc

compile:
	@$(REBAR) compile

edoc:
	@$(REBAR) edoc

test:
	@$(REBAR) test

clean:
	@$(REBAR) clean

dialyzer:
	@$(REBAR) dialyzer || $(REBAR) dialyzer

xref:
	@$(REBAR) xref

lint:
	@$(REBAR) lint
