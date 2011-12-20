REBAR=rebar
all: compile

dependencies: 
	@${REBAR} get-deps update-deps

clean:
	@${REBAR} clean

compile: 
	@${REBAR} compile

dialyze: compile
	@${REBAR} analyze

xref: compile
	@${REBAR} xref # don't scan dependencies...

check: compile xref dialyze test

doc:
	@${REBAR} doc

test: compile
	@${REBAR} eunit


