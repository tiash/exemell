% vim: cc=80 ft=erlang ts=2 sw=2 et
%
% Copyright 1992-2011 Matthias Horn. All rights reserved.
% 
% Redistribution and use in source and binary forms, with or without
% modification, are permitted provided that the following conditions are met:
% 
% 1. Redistributions of source code must retain the above copyright notice,
%    this list of conditions and the following disclaimer.
% 2. Redistributions in binary form must reproduce the above copyright notice,
%    this list of conditions and the following disclaimer in the documentation
%    and/or other materials provided with the distribution.
% 
% THIS SOFTWARE IS PROVIDED BY THE FREEBSD PROJECT "AS IS" AND ANY EXPRESS OR
% IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
% MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO
% EVENT SHALL THE FREEBSD PROJECT OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
% INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
% (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
% ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
% 
% The views and conclusions contained in the software and documentation are
% those of the authors and should not be interpreted as representing official
% policies, either expressed or implied, of Matthias Horn.

-module(exemell_benchmark).

-define(FILES
    , "some.xml"
    , "other.xml"
    ).
-define(RUNS,10).
-export([run/0,main/1,run_/0]).
main(_Args) ->
  run_().
run() ->
  c:c(?MODULE),
  ?MODULE:run_(),
  init:stop().
run_() ->
  Files =
    [ ?FILES
    ],
  io:format("Timings are microseconds per ~p runs.~n",[?RUNS]),
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
