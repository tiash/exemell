-module(specialise_transform).

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
  Specialisable = lists:flatten([ specialisable(Form) || Form <- ?unforms(Forms) ]),
  transform(?unforms(Forms),Specialisable,sets:from_list([])).
transform(Forms1,Specialisable,Specialisations1) ->
  {Forms2,Specialisations2} = lists:mapfoldl(usespecialised(Specialisable),Specialisations1,Forms1),
  case sets:to_list(sets:subtract(Specialisations2,Specialisations1)) of
    [] -> erl_syntax:revert_forms(?forms(Forms2));
    NewSpecialisations -> transform(?unforms([Forms2, [ specialise(S,Specialisable) || S <- NewSpecialisations]]),Specialisable,Specialisations2)
  end.

specialisable(Form) ->
  case erl_syntax:type(Form) of
    function ->
      % TODO: Check if the function can be specialised
      [];
    _ -> []
  end.

usespecialised(Specialisable) -> fun (E,Specs) -> usespecialised(E,Specialisable,Specs) end.

usespecialised(Expr1,Specialisable,Specialisations1) ->
  {Expr2,Specialisations2} = erl_syntax_lib:mapfold_subtrees(usespecialised(Specialisable),Specialisations1,Expr1),
  case erl_syntax:type(Expr2) of
    application ->
      % TODO: Check if this application can be replaced by a specialised variant
      {Expr2,Specialisations2};
    _ ->
      {Expr2,Specialisations2}
  end.

specialise({Fun,Args},Specialisations) -> 
  % TODO: Generate specialised versions...
  [].

name({Fun,Args}) ->
  % TODO: Generate specialised names....
  Fun.


