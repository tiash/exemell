#!/usr/bin/escript 
-export([run/0,main/1,run_/0]).
main(_Args) -> run_().
-define(RUNS,10).
run() ->
  c:c(?MODULE),
  ?MODULE:run_().
run_() ->
  code:add_path("ebin"),
  code:add_path("../ebin"),
  Files =
    [ "some.xml"
    ],
  io:format("Timings are for ~p runs.~n",[?RUNS]),
  [ begin
      io:format("input: ~s.~n",[File]),
      {ok,XML} = file:read_file(File),
      io:format("size: ~pkb (~pb).~n",[byte_size(XML) div 1024,byte_size(XML)]),
      run(exemell,xml,[XML]),
      TXT = binary_to_list(XML),
      run(xmerl_scan,string,[TXT]),
      run(erlsom,simple_form,[TXT])
    end || File <- Files
  ],
  io:format("Done.~n").

run(Module,Fun,Inputs) ->
  case code:which(Module) of
    non_existing -> 
      io:format("~p: unavailable.~n",[Module]);
    _ -> [begin
            {First,_} = timer:tc(ntimes(?RUNS,fun () -> Module:Fun(Input), ok end)),
            {Second,_} = timer:tc(ntimes(?RUNS,fun () -> Module:Fun(Input), ok end)),
            io:format("~p: ~pmicro seconds (~pmicro seconds).~n",[Module,Second,First])
          end || Input<-Inputs ]
  end.

ntimes(N,FUNC) -> fun () -> ntimes_(N,FUNC) end.
ntimes_(0,_) -> ok;
ntimes_(N,FUNC) -> FUNC(), ntimes_(N-1,FUNC).
