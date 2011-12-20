-module(exemell).

-export([xml/1]).
-export([userstate/1,userstate/2]).
-export([newNamespace/2,newNamespace/3,newNamespace/4]).
-export([namespace/2,namespace/3]).
-export([block/4,block/5,attribute/4]).

% -compile([inline,native,{hipe,[o3]}]).
-compile([native,{hipe,[o3]}]).
-compile([inline]).
% -compile([bin_opt_info]).

-include("core.hrl").



-record(global,
  { entities :: dict(binary() | {'%',binary()},term())
  , intern :: dict(binary(),binary())
  , namespaces :: dict(nsuri(),namespace())
  , application :: fun((iolist(),global()) -> global())
  , meta :: fun((iolist(),global()) -> global())
  , sections = 0 :: non_neg_integer()
  , userstate :: userstate()
  }).
-define(childfun(),fun((block()|close,userstate(),parser_state()) -> {userstate()|block(),parser_state()})).
-define(blockfuna(),fun((nsuri(),ptag(),[attribute()],parser_state()) -> 
                        ( {skip,parser_state()}
                        | {nochildren,block(),parser_state()}
                        | {blob,fun((iolist(),userstate(),parser_state())->{block(),parser_state()}),userstate(),parser_state()}
                        | {children,?childfun(),userstate(),parser_state()}))).
-define(blockfunb(),fun((nsuri(),ptag(),[attribute()],[block()]|none,parser_state()) -> {block()|skip,parser_state()})).
-define(blockfun(),?blockfuna()|?blockfunb()).
-define(attrfun(),fun((nsuri(),ptag(),iolist(),parser_state())->{attribute(),parser_state()})).
-record(namespace,
  { uri :: nsuri()
  , prefix :: binary()
  , block :: ?blockfun()
  , attribute :: ?attrfun()
  }).

-define(local, primary_ns :: namespace()
             , secondary_ns :: dict(binary(),namespace())
             , parent :: local()
             ).
-define(parent(Parent), primary_ns = primaryNamespace(?state{local=Parent}), secondary_ns = secondaryNamespaces(?state{local=Parent}), parent=Parent).
          
-record(attrs,{tag::tag(),attributes::[attribute()],?local}).
-record(block,{tag::tag(),function::?childfun(),state::userstate(),?local}).
-record(attr,{name::tag(),parent::local()}).
-record(attrval,{name::tag(),value::[term()],parent::local()}).

-type dict(_K,_V) :: dict().
-opaque global() :: #global{}.
-type local() :: #attr{} | #attrval{} | #attrs{} | #block{}.
-type namespace() :: #namespace{}.
-type block() :: term() | iolist().
-type attribute() :: term().
-type userstate() :: term().
-type nsuri() :: none | binary().
-type ptag() :: binary() | atom().
-type tag() :: ptag() | {binary(),ptag()}.
-record(?MODULE,{global::global(),local::local()}).
-define(state,#?MODULE).

-type parser_state() :: ?state{}.

-define(xml_nsuri,<<"http://www.w3.org/XML/1998/namespace">>).

xml(Input) -> xml(Input,newGlobalState()).
xml(Input,GlobalState) ->
  case parser(Input,newRootState(GlobalState)) of
    {ok,ParserState1=?state{local=#block{tag={'#ROOT'},function=Fun,state=State}}} ->
      {Res,ParserState2} = Fun(close,State,ParserState1),
      {Res,ParserState2?state.global};
    Err -> Err
  end.
newGlobalState() -> newGlobalState(undefined).
newGlobalState(UserState) -> newGlobalState(UserState,fun (_Application,GlobalState) -> GlobalState end, fun (_Meta,GlobalState) -> GlobalState end).
newGlobalState(UserState,Application,Meta) ->
  GlobalState1 = #global{entities=dict:new(),intern=dict:new(),namespaces=dict:new(),application=Application,meta=Meta,sections=0,userstate=UserState},
  ?state{global=GlobalState2} = entity(<<"nbsp">>,<<" ">>,
                                entity(<<"lt">>,<<"<">>,
                                entity(<<"gt">>,<<">">>,
                                entity(<<"amp">>,<<"&">>,
                                entity(<<"quot">>,<<"\"">>,
                                entity(<<"apos">>,<<"\'">>,
                                namespace(?xml_nsuri,newNamespace(<<"xml">>,?xml_nsuri),
                                namespace(none,newNamespace(none,none),?state{global=GlobalState1})))))))),
  GlobalState2.

newRootState(GlobalState) ->
  ParserState = ?state{global=GlobalState,local=#block{
      tag={'#ROOT'},
      function=fun (close,Accum,PS) -> {lists:reverse(Accum),PS};
                   (Child,Accum,PS) -> {[Child|Accum],PS}
               end,
      state=[],
      secondary_ns = dict:new(),
      parent={'#ROOT'}
    }},
  xmlns(<<"xml">>,?xml_nsuri,xmlns(none,ParserState)).


-spec userstate(parser_state()) -> userstate().
-spec userstate(userstate(),parser_state()) -> userstate().
userstate(?state{global=#global{userstate=UserState}}) -> UserState.
userstate(UserState,ParserState=?state{global=GlobalState}) ->
  ParserState?state{global=GlobalState#global{userstate=UserState}}.

-spec namespace(nsuri(),parser_state()) -> {namespace(),parser_state()}.
-spec namespace(nsuri(),namespace(),parser_state()) -> parser_state().
namespace(URI,ParserState=?state{global=#global{namespaces=Namespaces}}) ->
  case dict:find(URI,Namespaces) of
    {ok,Namespace} -> {Namespace,ParserState};
    error ->
      Namespace = newNamespace(none,URI),
      {Namespace,namespace(URI,Namespace,ParserState)}
  end.
namespace(URI,Namespace,ParserState=?state{global=GlobalState=#global{namespaces=Namespaces}}) ->
      ParserState?state{global=GlobalState#global{namespaces=dict:store(URI,Namespace,Namespaces)}}.

-spec newNamespace(binary()|none,nsuri()) -> namespace().
-spec newNamespace(binary()|none,nsuri(), ?blockfun()) -> namespace().
-spec newNamespace(binary()|none,nsuri(), ?attrfun(), ?blockfun()) -> namespace().
newNamespace(Prefix,URI) ->
  newNamespace(Prefix,URI,fun block/4).
newNamespace(Prefix,URI,BlockFun) ->
  newNamespace(Prefix,URI,fun attribute/4,BlockFun).
newNamespace(Prefix,URI,AttrFun,BlockFun) ->
  #namespace{uri=URI,prefix=Prefix,attribute=AttrFun,block=BlockFun}.

-spec primaryNamespace(parser_state()) -> namespace().
-spec primaryNamespace(namespace(),parser_state()) -> parser_state().
primaryNamespace(?state{local=#attrs{primary_ns=Namespace}}) -> Namespace;
primaryNamespace(?state{local=#block{primary_ns=Namespace}}) -> Namespace.
primaryNamespace(Namespace,ParserState=?state{local=LocalState=#attrs{}}) ->
  ParserState?state{local=LocalState#attrs{primary_ns=Namespace}};
primaryNamespace(Namespace,ParserState=?state{local=LocalState=#block{}}) ->
  ParserState?state{local=LocalState#block{primary_ns=Namespace}}.

-spec secondaryNamespace(binary(),parser_state()) -> namespace().
-spec secondaryNamespace(binary(),namespace(),parser_state()) -> parser_state().
-spec secondaryNamespaces(parser_state()) -> dict(binary(),namespace()).
secondaryNamespace(Prefix,ParserState) -> dict:fetch(Prefix,secondaryNamespaces(ParserState)).
secondaryNamespace(Prefix,Namespace,ParserState) ->
  secondaryNamespaces(dict:store(Prefix,Namespace,secondaryNamespaces(ParserState)),ParserState).
secondaryNamespaces(?state{local=#attrs{secondary_ns=Namespaces}}) -> Namespaces;
secondaryNamespaces(?state{local=#block{secondary_ns=Namespaces}}) -> Namespaces.
secondaryNamespaces(Namespaces,ParserState=?state{local=LocalState=#attrs{}}) -> ParserState?state{local=LocalState#attrs{secondary_ns=Namespaces}};
secondaryNamespaces(Namespaces,ParserState=?state{local=LocalState=#block{}}) -> ParserState?state{local=LocalState#block{secondary_ns=Namespaces}}.

-spec dict:fetch(K,dict(K,V)) -> V.
-spec dict:find(K,dict(K,V)) -> {ok,V} | error.
-spec dict:store(K,V,dict(K,V)) -> dict(K,V).
-spec dict:from_list([{K,V}]) -> dict(K,V).
-spec dict:new() -> dict(any(),any()).

% -spec entity(binary()|{'%',binary()},parser_state()) -> term().
-spec entity(binary()|{'%',binary()},parser_state()) -> {term(),parser_state()}.
-spec entity(binary()|{'%',binary()},term(),parser_state()) -> parser_state().
entity(Name,ParserState=?state{global=#global{entities=Entities}}) ->
  case dict:find(iolist_to_binary(Name),Entities) of
    {ok,Res} -> {Res,ParserState};
    error ->
      Entity = case Name of
                <<"#x",Hex/bytes>> ->
                  <<(list_to_integer(binary_to_list(Hex),16))/utf8>>;
                <<"#",Dec/bytes>> ->
                  <<(list_to_integer(binary_to_list(Dec),10))/utf8>>
               end,
      {Entity,entity(Name,Entity,ParserState)}
  end.
entity(Name,Value,ParserState=?state{global=GlobalState=#global{entities=Entities}}) ->
  ParserState?state{global=GlobalState#global{entities=dict:store(Name,Value,Entities)}}.

-spec intern(iolist(),parser_state()) -> {binary(),parser_state()}.
-spec name(iolist(),parser_state()) -> {atom()|binary(),parser_state()}.
% intern(Value,ParserState) -> {Value,ParserState}.
intern(Value,ParserState=?state{global=Global=#global{intern=Interned}}) when is_binary(Value) ->
  case dict:find(Value,Interned) of
    {ok,IValue} -> {IValue,ParserState};
    error ->
      IValue = binary:copy(Value), %% Check if Value is a subbinary
      {IValue,ParserState?state{global=Global#global{intern=dict:store(IValue,IValue,Interned)}}}
  end;
intern(Value,ParserState) -> intern(iolist_to_binary(Value),ParserState).
name(Name,ParserState) when is_atom(Name) -> {Name,ParserState};
name(Name,ParserState) when is_binary(Name) -> 
  try {binary_to_existing_atom(Name,utf8),ParserState}
  catch
    error:badarg -> intern(Name,ParserState)
  end;
name(Name,ParserState) -> name(iolist_to_binary(Name),ParserState).
        

-spec addChild(block(),parser_state()) -> parser_state().
addChild(Child,ParserState=?state{local=#block{function=Fun,state=UserState}}) ->
  {NewUserState,NewParserState} = Fun(Child,UserState,ParserState),
  NewParserState?state{local=NewParserState?state.local#block{state=NewUserState}}.

-spec addAttribute(tag(),iolist(),parser_state()) -> parser_state().
addAttribute(xmlns,Value,ParserState) ->
  % io:format("addAttribute(xmlns,~p,...)~n",[Value]),
  xmlns(Value,ParserState);
addAttribute({<<"xmlns">>,Prefix_},Value,ParserState) ->
  % io:format("addAttribute({xmlns,~p},~p,...)~n",[Prefix_,Value]),
  if is_atom(Prefix_) -> Prefix=atom_to_binary(Prefix_,utf8);
     is_binary(Prefix_) -> Prefix = Prefix_
  end,
  xmlns(Prefix,Value,ParserState);
addAttribute(Name,Value,ParserState=?state{local=(LocalState=#attrs{attributes=Attributes})}) ->
  % io:format("addAttribute(~p,~p,...)~n",[Name,Value]),
  ParserState?state{local=LocalState#attrs{attributes=[{Name,Value}|Attributes]}}.

% -spec attribute(nsuri(),ptag(),iolist(),parser_state()) -> {{tag(),iolist()}|skip,parser_state()}.
% attribute(none,Name,Value,ParserState) ->
  % {{Name,Value},ParserState};
% attribute(Uri,Name,Value,ParserState) ->
  % {{{Uri,Name},Value},ParserState}.

-spec block(nsuri(),ptag(),[attribute()],[block()]|none,parser_state()) -> {{tag(),[attribute()],[block()]},parser_state()}.
-spec block(nsuri(),ptag(),[attribute()],parser_state()) -> {children,?childfun(),{tag(),[attribute()],[block()]},parser_state()}.
-spec children(block()|close,{tag(),[attribute()],[block()]},parser_state()) -> {{tag(),[attribute()],[block()]},parser_state()}.
block(none,Tag,Attributes,Children,ParserState) ->
  {{Tag,Attributes,Children},ParserState};
block(Uri,Tag,Attributes,Children,ParserState) ->
  {{{Uri,Tag},Attributes,Children},ParserState}.
block(none,Tag,Attributes,ParserState) ->
  {children,fun children/3,{Tag,Attributes,[]},ParserState};
block(Uri,Tag,Attributes,ParserState) ->
  {children,fun children/3,{{Uri,Tag},Attributes,[]},ParserState}.
children(close,{Tag,Attributes,Children},ParserState) ->
  {{Tag,Attributes,lists:reverse(Children)},ParserState};
children(Child,{Tag,Attributes,Children},ParserState) ->
  {{Tag,Attributes,[Child,Children]},ParserState}.

-spec attribute(nsuri(),ptag(),iolist(),parser_state()) -> {attribute(),parser_state()}.
attribute(none,Name,Value,ParserState) -> {{Name,Value},ParserState};
attribute(NS,Name,Value,ParserState) -> {{{NS,Name},Value},ParserState}.

-spec xmlns(ptag(),iolist(),parser_state()) -> parser_state().
xmlns(Name,Value,ParserState) when is_atom(Name) ->
  {BName,ParserState2} = intern(atom_to_binary(Name,utf8),ParserState),
  xmlns(BName,Value,ParserState2);
xmlns(Name,none,ParserState) ->
  {Namespace,ParserState2} = namespace(none,ParserState),
  secondaryNamespace(Name,Namespace,ParserState2);
xmlns(Name,Value,ParserState) ->
  {Namespace,ParserState2} = namespace(iolist_to_binary(Value),ParserState),
  secondaryNamespace(Name,Namespace,ParserState2).
-spec xmlns(iolist(),parser_state()) -> parser_state().
xmlns(none,ParserState) ->
  {Namespace,ParserState2} = namespace(none,ParserState),
  primaryNamespace(Namespace,ParserState2);
xmlns(Value,ParserState) ->
  {Namespace,ParserState2} = namespace(iolist_to_binary(Value),ParserState),
  primaryNamespace(Namespace,ParserState2).

-spec nsify(iolist(),parser_state()) -> {tag(),parser_state()}.
nsify(Input,State) when is_atom(Input) -> {Input,State};
nsify(Input_,State1) ->
  Input = iolist_to_binary(Input_),
  L=byte_size(Input),
  case binary:match(Input,<<":">>) of
    nomatch -> 
      {Name,State2} = name(Input,State1),
      {Name,State2};
    {S,1} ->
      {Prefix,State2} = intern(binary_part(Input,0,S),State1),
      {Name,State3} = name(binary_part(Input,S+1,L-S-1),State2),
      {{Prefix,Name},State3}
  end.

?event_text(Text,{ParserState,Input}) ->
  {addChild(Text,ParserState),Input}.
?event_entity(Name,{ParserState1,Input}) ->
  {Entity,ParserState2} = entity(Name,ParserState1),
  {addChild(Entity,ParserState2),Input}.
?event_comment(_Comment,{ParserState,Input}) ->
  {ParserState,Input}.
?event_application(Application,{ParserState=?state{global=#global{application=CB}},Input}) ->
  {CB(Application,ParserState),Input}.
?event_meta(Meta,{ParserState=?state{global=#global{meta=CB}},Input}) ->
  {CB(Meta,ParserState),Input}.
?event_open_tag(RTag,{ParserState1,Input}) ->
  % io:format("OTag=~p~n",[RTag]),
  {Tag,ParserState2=?state{local=LocalState}} = nsify(RTag,ParserState1),
  {ParserState2?state{local=#attrs{tag=Tag,attributes=[],?parent(LocalState)}},Input}.

?event_end_block(CTag_,{ParserState1,Input}) ->
  % io:format("CTag = ~p~n",[CTag_]),
  {CTag,ParserState2=?state{local=#block{tag=Tag,function=Fun,state=UserState}}} = nsify(CTag_,ParserState1),
  {Block,ParserState3} = Fun(close,UserState,ParserState2),
  ParserState4 = addChild(Block,ParserState3?state{local=ParserState3?state.local#block.parent}),
  case CTag of
    Tag -> {ParserState4,Input};
    _ ->
      ?event_end_block(CTag,{ParserState4,Input})
  end.

?event_attribute_name(Name_,{ParserState1,Input}) ->
  {Name,ParserState2=?state{local=Parent}} = nsify(Name_,ParserState1),
  {ParserState2?state{local=#attr{name=Name,parent=Parent}},Input}.

preclose_tag(ParserState1=?state{local=#attrs{tag=Tag,attributes=Attrs_}}) ->
  {Attrs,ParserState2} = process_attrs(lists:reverse(Attrs_),ParserState1,[]),
  {Tag,Attrs,ParserState2}.

process_attrs([],ParserState,Accum) -> {lists:reverse(Accum),ParserState};
process_attrs([{Name,Value}|Attrs],ParserState1,Accum) ->
  case Name of
    {Prefix,PName} ->
      #namespace{uri=Uri,attribute=Fun} = secondaryNamespace(Prefix,ParserState1);
    PName ->  
      Uri = none,
      #namespace{attribute=Fun} = primaryNamespace(ParserState1)
  end,
  case Fun(Uri,PName,Value,ParserState1) of
    {skip,ParserState2} -> process_attrs(Attrs,ParserState2,Accum);
    {Attr,ParserState2} -> process_attrs(Attrs,ParserState2,[Attr|Accum])
  end.
  
  

?event_close_tag_begin_block({ParserState1,Input}) ->
  {Tag,Attrs,ParserState2} = preclose_tag(ParserState1),
  % io:format("tag/enter[1] ~p~n",[ParserState2]),
  % io:format("tag/enter[1].tag ~p~n",[Tag]),
  % io:format("tag/enter[1].nss ~p~n",[dict:to_list(secondaryNamespaces(ParserState2))]),
  % io:format("tag/enter[1].attrs ~p~n",[Attrs]),
  case Tag of
    {Prefix,PTag} ->
      #namespace{uri=Uri,block=Fun_} = secondaryNamespace(Prefix,ParserState2);
    PTag -> 
      #namespace{uri=Uri,block=Fun_} = primaryNamespace(ParserState2)
  end,
  if is_function(Fun_,4) ->
      Fun=Fun_;
     is_function(Fun_,5) ->
      Fun = fun (_Uri,_PTag,_Attrs,_PS) -> 
                {children,fun (close,{__Uri,__PTag,__Attrs,__Children},__PS) ->
                                Fun_(__Uri,__PTag,__Attrs,lists:reverse(__Children),__PS);
                              (Child,{__Uri,__PTag,__Attrs,__Children},__PS) ->
                                {{__Uri,__PTag,__Attrs,[Child|__Children]},__PS}
                          end,{_Uri,_PTag,_Attrs,[]},_PS}
            end
  end,
  case Fun(Uri,PTag,Attrs,ParserState2) of
    {skip,ParserState3} -> erlang:exit(todo), {ParserState3,Input};
    {blob,_BFun,_State,_ParserState3=?state{local=_Parent}} ->
      erlang:exit(todo);
    {children,CFun,State,ParserState3=?state{local=#attrs{parent=Parent,primary_ns=Primary,secondary_ns=Secondary}}} ->
      {ParserState3?state{local=#block{tag=Tag,function=CFun,state=State,parent=Parent,primary_ns=Primary,secondary_ns=Secondary}},Input}
  end.


%% TODO: i'm here...
  
?event_close_tag({ParserState1,Input}) ->
  % io:format("close/tag[1]"),
  {Tag,Attrs,ParserState2} = preclose_tag(ParserState1),
  % io:format("close/tag[2]"),
  case Tag of
    {Prefix,PTag} ->
      #namespace{uri=Uri,block=Fun} = secondaryNamespace(Prefix,ParserState2);
    PTag -> 
      #namespace{uri=Uri,block=Fun} = primaryNamespace(ParserState2)
  end,
  % io:format("close/tag[3]"),
  if is_function(Fun,4) ->
        case Fun(Uri,PTag,Attrs,ParserState2) of
          {skip,ParserState3=?state{local=#attrs{parent=Parent}}} -> {ParserState3?state{local=Parent},Input};
          {blob,BFun,State,ParserState3} ->
            % io:format("close/tag[4]"),
            {Node,ParserState4=?state{local=#attrs{parent=Parent}}} = BFun(none,State,ParserState3),
            % io:format("close/tag[5]"),
            {addChild(Node,ParserState4?state{local=Parent}),Input};
          {children,CFun,State,ParserState3} ->
            % io:format("close/tag[6]"),
            {Node,ParserState4=?state{local=#attrs{parent=Parent}}} = CFun(close,State,ParserState3),
            % io:format("close/tag[7]"),
            {addChild(Node,ParserState4?state{local=Parent}),Input}
        end;
     is_function(Fun,5) ->
        % io:format("close/tag[8]"),
        {Node,ParserState3=?state{local=#attrs{parent=Parent}}} = Fun(Uri,PTag,Attrs,none,ParserState2),
        % io:format("close/tag[9]"),
        {addChild(Node,ParserState3?state{local=Parent}),Input}
  end.

?event_attribute_begin_value({ParserState=?state{local=#attr{name=Name,parent=Parent}},Input}) ->
  {ParserState?state{local=#attrval{name=Name,value=[],parent=Parent}},Input}.
?event_attribute_value(Value,{ParserState=?state{local=#attr{name=Name,parent=Parent}},Input}) ->
  {addAttribute(Name,Value,ParserState?state{local=Parent}),Input}.

?event_attribute_end_value({ParserState=?state{local=#attrval{name=Name,value=Value,parent=Parent}},Input}) ->
  {addAttribute(Name,lists:reverse(Value),ParserState?state{local=Parent}),Input}.
?event_attribute_value_text(Text,{ParserState=?state{local=Local=#attrval{value=Value}},Input}) ->
  {ParserState?state{local=Local#attrval{value=[Text|Value]}},Input}.
?event_attribute_value_entity(Name,{ParserState1,Input}) ->
  {Entity,ParserState2=?state{local=Local=#attrval{value=Value}}} = entity(Name,ParserState1),
  {ParserState2?state{local=Local#attrval{value=[Entity|Value]}},Input}.

?event_end_section({ParserState,Input}) ->
  {decrement_section(ParserState),Input}.
?event_begin_section(Name_,{ParserState1,Input1}) ->
  Name = iolist_to_binary(Name_),
  Len = byte_size(Name),
  ILen = Len-2,
  case Name of
    <<"CDATA">> -> 
      {CData,Input2} = scan_section(Input1),
      {addChild(CData,ParserState1),Input2};
    <<"INCLUDE">> -> {increment_section(ParserState1),Input1};
    <<"IGNORE">> -> {ParserState1,ignore_sections(Input1,1)};
    <<$%,NName:(ILen)/bytes,$;>> ->
        {Entity,ParserState2} = entity({'%',NName},ParserState1),
        ?event_begin_section(Entity,{ParserState2,Input1});
    _ -> erlang:error({unsupported_sction,Name})
  end.


decrement_section(_State) -> erlang:error(todo).
increment_section(_State) -> erlang:error(todo).

ignore_sections(Input,0) -> Input;
ignore_sections(Input,N) -> ignore_sections_(Input,N).
ignore_sections_(Input,N) -> parser:input(Input);
ignore_sections_(<<_,"]]>",Input>>,N) ->
  ignore_sections(Input,N-1);
ignore_sections_(<<_,"<![",Input>>,N) ->
  ignore_sections(Input,N+1).

scan_section(Input) -> parser:input(Input);
scan_section(<<Section,"]]>",Input>>) -> {Section,Input}.


% revflatten(A) -> flatten(lists:reverse(A)).
% revfflatten(A) -> fflatten(lists:reverse(A)).

% flatten(A) -> flatten(A,[]).
% flatten([],Accum) -> lists:reverse(Accum);
% flatten([[]|A],Accum) -> flatten(A,Accum);
% flatten([[A]|B],Accum) -> flatten([A|B],Accum);
% flatten([[A|B]|C],Accum) -> flatten([A,B|C],Accum);
% flatten([A|B],Accum) -> flatten(B,[A|Accum]);
% flatten(A,Accum) -> flatten([],[A|Accum]).
% fflatten(A) -> case flatten(A) of [X] -> X; X->X end.

  
  


