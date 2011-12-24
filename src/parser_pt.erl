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
% 

-module(parser_pt).

-export([parse_transform/2]).

%% to use this parse transform simply ensure the first clause of you function is:
%%      Name(Args...) -> parser:input(Arg);
%% followed by clauses where Arg is replaced by a binary expr, of the form, <<Before,Pat,After>> where Pat is a string or a list of (alternative) strings.


-define(debug(Value),(fun (__V__) -> ?debug("~s = ~p",[??Value,__V__]),__V__ end)(Value)).
% -define(debug(Pattern,Args),
  % io:format("~s:~p   "++Pattern++"~n",[?FILE,?LINE]++Args)).
-define(debug(Pattern,Args), ok).

-define(forms(F), erl_syntax:flatten_form_list(erl_syntax:form_list(lists:flatten([F])))).
-define(unforms(F), erl_syntax:form_list_elements(?forms(F))).

-define(atom(Atom),erl_syntax:atom(Atom)).
-define(var(Var),erl_syntax:variable(Var)).
-define(var(Prefix,Name),?var(list_to_atom(atom_to_list(Prefix)++"_"++atom_to_list(Name)))).
-define(apply(Fun,Args),erl_syntax:application(?atom(Fun),Args)).
-define(apply(Mod,Fun,Args),erl_syntax:application(?atom(Mod),?atom(Fun),Args)).
-define(clause(Pattern,Guard,Body),erl_syntax:clause(Pattern,Guard,Body)).
-define(cases(Arg,Clauses),erl_syntax:case_expr(Arg,Clauses)).
-define(ifs(Clauses),erl_syntax:if_expr(Clauses)).
-define(record(Value,Name,Fields),erl_syntax:record_expr(Value,?atom(Name),Fields)).
-define(record(Name,Fields),erl_syntax:record_expr(?atom(Name),Fields)).
-define(field(Name),erl_syntax:record_field(?atom(Name))).
-define(field(Name,Value),erl_syntax:record_field(?atom(Name),Value)).
-define(int(Val),erl_syntax:integer(Val)).
-define(underscore,erl_syntax:underscore()).
-define(tuple(Elems),erl_syntax:tuple(Elems)).
-define(function(Name,Clauses),erl_syntax:function(?atom(Name),Clauses)).
-define(match(Left,Right),erl_syntax:match_expr(Left,Right)).
-define(abstract(Term),erl_syntax:abstract(Term)).
-define(cons(Head,Tail),erl_syntax:cons(Head,Tail)).
-define(list(Elems),erl_syntax:list(Elems)).
-define(list(Elems,Tail),erl_syntax:list(Elems,Tail)).
-define(nil,erl_syntax:nil()).
-define(func(Clauses),erl_syntax:fun_expr(Clauses)).
-define(access(Value,Record,Field),erl_syntax:record_access(Value,?atom(Record),?atom(Field))).
-define(suppress_unused(Fun,Arity),erl_syntax:attribute(?atom(compile),[ ?abstract({nowarn_unused_function,{Fun,Arity}}) ])).


parse_transform(Forms,_Options) ->
  erl_syntax:revert_forms(?forms([ transform(Form) || Form <- ?unforms(Forms) ])).

transform(Form) ->
  Res = 
  case erl_syntax:type(Form) of
    function ->
      {Name,Arity} = erl_syntax_lib:analyze_function(Form),
      ?debug(Name),
      case [ {erl_syntax:clause_patterns(C),erl_syntax:clause_guard(C),erl_syntax:clause_body(C)} || C <- erl_syntax:function_clauses(Form)] of
        [{Args,none,[Parser]}|Patterns] ->
          ?debug(Args),
          ?debug(Parser),
          ?debug(Patterns),
          case marker(Parser) of
            {MatchMod,Input} ->
              ?forms(transform(MatchMod,Name,Arity,pos(Input,Args),Patterns));
            _ -> Form
          end;
        _ -> Form
      end;
    _ -> Form
  end,
  % RRes = if not(is_list(Res)) ->
  % case erl_syntax:type(Res) of
  % attribute ->
      % case erl_syntax:atom_value(erl_syntax:attribute_name(Res)) of
        % type -> [];
        % opaque -> [];
        % spec -> [];
        % _ -> Res
      % end;
  % _ -> Res
  % end; true -> Res end,
  % io:format("~s~n~n",[erl_prettypr:format(?forms([RRes]))]),
  Res.

transform(MatchMod,Name,Arity,InputIX,Clauses1) ->
  Args1 = [var(N) || N<-lists:seq(1,InputIX-1)], Args2 = [var(N) || N<-lists:seq(InputIX+1,Arity)],
  Args = Args1 ++ [?var('Input')] ++ Args2,
  Patterns =
    orddict:update(terminal,fun (X) -> X end,
    [{[?underscore || _<-Args1] ++ [?underscore] ++ [?underscore || _<-Args2],[],[?tuple([?atom(error),?atom(eof)])]}],
    group([ begin Input = lists:nth(InputIX,CArgs),
            ?debug(Input),
            case erl_syntax:type(Input) of
              binary ->
                case [ {erl_syntax:binary_field_body(F),erl_syntax:binary_field_size(F),erl_syntax:binary_field_types(F)} || F <- erl_syntax:binary_fields(Input)] of
                  [{Before,none,[]},{Match,none,[]},{After,none,[]}] ->
                    
                    tags(erl_syntax:concrete(Match),{lists:sublist(CArgs,InputIX-1) ++ [Before,After] ++ lists:nthtail(InputIX,CArgs),Guards,Body});
                  _ -> {terminal,{CArgs,Guards,Body}}
                  % Err -> io:format("Huh? ~s~n~p",[erl_prettypr:format(Err),Err]), err
                end;
              _ -> {terminal,{CArgs,Guards,Body}}
            end end || {CArgs,Guards,Body} <- Clauses1])),
  StopWords = [ S || S <- orddict:fetch_keys(Patterns), is_binary(S) ],
  % StopWordsLength = lists:max( [0 | [ byte_size(S) || S <-StopWords ]] ),
  [ ?function(Name,[?clause(Args,[],
      [ case MatchMod of
          % iolist -> ?apply(iolist,match,[?var('Input'),?apply(binary,compile_pattern,[?abstract(StopWords)]),?abstract(StopWordsLength)]);
          binary -> ?apply(name(Name,match),Args1++[?var('Input')]++Args2)
        end]
      )])
  , matchFun(Name,lists:reverse(lists:sort(StopWords)),Args1,Args2)
  | [ [ erl_syntax:attribute(?atom(compile),[?abstract([{inline,{name(Name,Terminal),Arity+case Terminal of terminal -> 0; _ -> 1 end}}])])
      , ?function(name(Name,Terminal), [ ?clause(CArgs,Guard,Body)  || {CArgs,Guard,Body} <- Stop ])
      ] || {Terminal,Stop} <- Patterns ] ].

%% Generates a matching function of form...
%% fun#match(<<Prefix:Consumed/bytes,"Pat"/utf8,Rest/bytes>>,Consumed,Args) -> 
%%    fun#Pat#(Preifx,Rest,Args);
%% fun#match(Input,Consumed,Args) when Consumed < byte_size(Input) -> fun#match(Input,Consumed+1,Args);
%% fun#match(Input,_Consumed,Args) -> fun#terminal(Input,Args).
-define(TRIE_MATCH,true). %% Use a trie
% -define(TRIE_A,true). %% and don't do the naive input spliting
-ifdef(TRIE_MATCH).
-ifdef(TRIE_A).
matchFun(Name,Stops,Args1,Args2) ->
  [
  ?function(name(Name,match),[?clause(Args1++[?var('Input')]++Args2,[?apply(is_binary,[?var('Input')])],[?apply(name(Name,match),[?var('Input'),?int(0)]++Args1++Args2)])]),
  ?function(name(Name,next),[?clause([?var('Input'),?var('Pos')]++Args1++Args2,none,[?ifs([
    ?clause([],[erl_syntax:infix_expr(?var('Pos'),erl_syntax:operator('<'),?apply(byte_size,[?var('Input')]))],
      [?apply(name(Name,match),[?var('Input'),erl_syntax:infix_expr(?var('Pos'),erl_syntax:operator('+'),?int(1))]++Args1++Args2)]),
    ?clause([],[?atom(true)],[?apply(name(Name,terminal),Args1++[?var('Input')]++Args2)])])])]),
  ?function(name(Name,match),[?clause([?var('Input'),?var('Pos')]++Args1++Args2,none,[
        ?match(erl_syntax:binary([erl_syntax:binary_field(?var('Left'),?var('Pos'),[?atom(bytes)]),erl_syntax:binary_field(?var('Right'),[?atom(bytes)])]),?var('Input')),
        matchTrie(trie(Stops),?var('Left'),?var('Right'),sets:new(),
        fun (Stop,Before,After) -> ?apply(name(Name,Stop),Args1++[Before,After]++Args2) end,
        ?apply(name(Name,next),[?var('Input'),?var('Pos')]++Args1++Args2))])])
  ].
-else.
matchFun(Name,Stops,Args1,Args2) ->
[
  ?function(name(Name,match),[?clause(Args1++[?var('Input')]++Args2,[?apply(is_binary,[?var('Input')])],[?apply(name(Name,match),[?var('Input'),?var('Input'),?int(0)]++Args1++Args2)])]),
  ?function(name(Name,match),
    [ ?clause([erl_syntax:binary([erl_syntax:binary_field(?int(K)) || K<-Ks]++[erl_syntax:binary_field(?var('More'),[?atom(bytes)])]),?var('Input'),?var('Pos')]++Args1++Args2,none,
        [matchTrie(R,?apply(binary_part,[?var('Input'),?int(0),?var('Pos')]),?var('More'),sets:new(),
          fun (Stop,Before,After) -> ?apply(name(Name,Stop),Args1++[Before,After]++Args2) end,
          ?apply(name(Name,match),[?var('More'),?var('Input'),erl_syntax:infix_expr(?var('Pos'),erl_syntax:operator('+'),?int(1))]++Args1++Args2))]) || {Ks,R} <- trie(Stops) ]
     ++
    [?clause([erl_syntax:binary([erl_syntax:binary_field(?underscore),erl_syntax:binary_field(?var('More'),[?atom(bytes)])]),?var('Input'),?var('Pos')]++Args1++Args2,none,
      [?apply(name(Name,match),[?var('More'),?var('Input'),erl_syntax:infix_expr(?var('Pos'),erl_syntax:operator('+'),?int(1))]++Args1++Args2)])
    ,?clause([erl_syntax:binary([]),?var('Input'),?underscore]++Args1++Args2,none,[?apply(name(Name,terminal),Args1++[?var('Input')]++Args2)])]
  )].
-endif.

trie(As) -> trie0(trie___(trie__([trie_(binary_to_list(A),A) || A<-As]))).
trie_([],R) -> [R];
trie_([C|CS],R) -> [{[C],trie_(CS,R)}].
trie__(AS) ->
  lists:foldl(
    fun({K,V}, Dict) ->
      case trie_find(K,Dict) of
        {ok,Vs} -> trie_put(K,trie__(V++Vs),Dict);
        false -> trie_put(K,V,Dict)
      end;
       (R,Dict) -> [R|Dict]
    end,[],lists:flatten([AS])).
% trie___(A) -> A.
trie___([{A,[{B,C}]}|D]) -> trie___([{A++B,C}|D]);
% trie___([{A,[{B,C}]}]) -> trie___([{A++B,C}]);
trie___([{A,B}|D]) -> [{A,trie__(B)}|trie___(D)];
trie___([]) -> [];
trie___(Bin) when is_binary(Bin) -> Bin.
% trie___(As) -> [ case A of {X,Y} -> {X,trie___(Y)}; _ -> A end || A<-As ].

trie0([{[A|As=[_|_]],B}|C]) -> [{[A],[{As,B}]}|trie0(C)];
trie0([{A=[_],B}|C]) -> [{A,B}|trie0(C)];
trie0([]) -> [].

trie_find(_K,[]) -> false;
trie_find(K,[{K,V}|_]) -> {ok,V};
trie_find(K,[_|D]) -> trie_find(K,D).
trie_put(K,V,D) -> lists:sort([{K,V}|trie_del(K,D)]).
trie_del(_K,[]) -> [];
trie_del(K,[{K,_}|D]) -> trie_del(K,D);
trie_del(K,[H|T]) -> [H|trie_del(K,T)].

matchTrie([RS],Before,After,_Vars,Clause,_Fail) when is_binary(RS) -> Clause(RS,Before,After);
matchTrie(RS_,Before,After1,Vars,Clause,Fail) ->
  case lists:partition(fun is_binary/1,RS_) of
    {[],RS} ->
      After2 = erl_syntax_lib:new_variable_name(Vars),
      Res = 
      ?cases(After1,[?clause([erl_syntax:binary([erl_syntax:binary_field(?int(K)) || K<-Ks] ++ [erl_syntax:binary_field(?var(After2),[?atom(bytes)])])],none,
        [matchTrie(R,Before,?var(After2),sets:add_element(After2,Vars),Clause,Fail)]) || {Ks,R} <- RS] ++ [?clause([?underscore],none,[Fail])]),
      % io:format("Res = ~p~n",[Res]),
      % io:format("~s~n~n",[erl_prettypr:format(Res)]),
      Res;
    {[F],RS} -> matchTrie(RS,Before,After1,Vars,Clause,Clause(F,Before,After1))
  end.


-else.
matchFun(Name,Stops,Args1,Args2) ->
  [
  ?function(name(Name,match),[?clause(Args1++[?var('Input')]++Args2,[?apply(is_binary,[?var('Input')])],[?apply(name(Name,match),[?var('Input'),?int(0)]++Args1++Args2)])]),
  ?function(name(Name,match),[?clause([?var('Input'),?var('Pos')]++Args1++Args2,none,
    [?match(?var('Len'),?apply(byte_size,[?var('Input')])),?cases(?var('Input'),
      [?clause([erl_syntax:binary([erl_syntax:binary_field(?var('Left'),?var('Pos'),[?atom(bytes)])]++[erl_syntax:binary_field(?int(K)) || K<-binary_to_list(Stop)]++[erl_syntax:binary_field(?var('Right'),[?atom(bytes)])])],none,[?apply(name(Name,Stop),Args1++[?var('Left'),?var('Right')]++Args2)]) || Stop <- Stops ]
      ++ [?clause([?underscore], [erl_syntax:infix_expr(?var('Pos'),erl_syntax:operator('<'),?var('Len'))],
      [?apply(name(Name,match),[?var('Input'),erl_syntax:infix_expr(?var('Pos'),erl_syntax:operator('+'),?int(1))]++Args1++Args2)]),
      ?clause([?underscore],[],[?apply(name(Name,terminal),Args1++[?var('Input')]++Args2)])])])])
  ].
-endif.


var(N) -> ?var(list_to_atom("Arg"++integer_to_list(N))).
name(Name,Atom) when is_atom(Atom) -> list_to_atom(atom_to_list(Name)++"%"++atom_to_list(Atom));
name(Name,Stop) when is_binary(Stop) -> list_to_atom(atom_to_list(Name)++"%\""++binary_to_list(Stop)++"\"").
  
tags(Str,Clause) when is_binary(Str) -> {Str,Clause};
tags(Str,Clause) when is_list(Str) ->
  case lists:all(fun is_integer/1,Str) of
    true -> {unicode:characters_to_binary(Str),Clause};
    _ -> [tags(S,Clause) || S<-Str]
  end.

group(In) -> lists:foldl(fun ({Tag,Val},Out) -> orddict:append(Tag,Val,Out) end,[],lists:flatten(In)).

marker(Form) ->
  case erl_syntax:type(Form) of
    application ->
      case erl_syntax_lib:analyze_application(Form) of
        {parser,{binary,1}} -> {binary,hd(erl_syntax:application_arguments(Form))};
        _ -> false
      end;
    _ -> false
  end.

pos(_,[]) -> erlang:error(not_foud);
pos(P,[H|T]) ->
  case in(P,H) of
    true -> 1;
    _ -> 1+pos(P,T)
  end.
in(_,[]) -> false;
in(A,[H|T]) -> in(A,H) orelse in(A,T);
in(A,A) -> true;
in(A_,B_) ->
  A = erl_syntax:remove_comments(A_),
  B = erl_syntax:remove_comments(erl_syntax:copy_attrs(A,B_)),
  A==B orelse in(A,erl_syntax:subtrees(B)).


