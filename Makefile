REBAR=rebar
DIALYZER=dialyzer
all: compile

dependencies: 
	@${REBAR} get-deps update-deps

clean:
	@${REBAR} clean

compile: 
	@${REBAR} compile


xref: compile
	@${REBAR} xref # don't scan dependencies...

check: compile xref dialyze test

doc:
	@${REBAR} doc

test: compile
	@${REBAR} eunit

.plt:
	@${DIALYZER} --build_plt --output_plt plt --apps erts kernel stdlib
plt: .plt

dialyze: plt
	@${DIALYZER} --plt plt --src -I include -r src



