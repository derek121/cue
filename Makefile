all: compile

compile:
	@rebar3 compile

shell:
	@rebar3 shell

release:
	@rebar3 as prod release

test:
	@rebar3 eunit

