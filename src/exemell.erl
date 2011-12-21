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

-module(exemell).

-export([xml/1,xml/2,parser/2]).
-export([userstate/1,userstate/2]).
-export([newNamespace/2,newNamespace/3,newNamespace/4]).
-export([namespace/2,namespace/3]).
-export([block/4,block/5,attribute/4]).

% -compile([inline,native,{hipe,[o3]}]).
-compile([native,{hipe,[o3]}]).
-compile([inline]).
% -compile([bin_opt_info]).


-export_type([state/3]).
-define(entrypoint,run_parser).

-include("exemell.hrl").
-include("parser.hrl").



-record(global, {entities, intern, namespaces, application, meta, sections = 0, userstate }).
-opaque global(Block,Attr,User) ::
  #global
  { entities :: dict(binary() | {'%',binary()},term())
  , intern :: dict(binary(),binary())
  , namespaces :: dict(nsuri(),namespace(Block,Attr,User))
  , application :: applicationFun(Block,Attr,User)
  , meta :: metaFun(Block,Attr,User)
  , sections :: non_neg_integer()
  , userstate :: User
  }.
-define(local, primary_ns, secondary_ns, parent).
-define(parent(Parent), primary_ns = primaryNamespace(?state{local=Parent}), secondary_ns = secondaryNamespaces(?state{local=Parent}), parent=Parent).
          
-record(local_attrs,{tag, attributes, primary_ns, secondary_ns, parent}).
-type local_attrs(Block,Attribute,Global) ::
  #local_attrs
  { tag :: binary()
  , attributes :: [{binary(), value()}]
  , primary_ns :: namespace(Block,Attribute,Global)
  , secondary_ns :: dict(binary(), namespace(Block,Attribute,Global))
  , parent :: local_block(Block,Attribute,Global)
  }.

-record(local_attr,{name,parent}).

-type local_attr(Block,Attribute,Global) ::
  #local_attr
  { name :: binary()
  , parent :: local_attrs(Block,Attribute,Global)
  }.

-record(local_attrval,{name,value,parent}).

-type local_attrval(Block,Attribute,Global) ::
  #local_attrval
  { name :: binary()
  , value :: value()
  , parent :: local_attrs(Block,Attribute,Global)
  }.

-record(local_block,{tag, function, state, primary_ns, secondary_ns, parent}).

-type local_block(Block,Attribute,User) ::
  #local_block
  { tag :: binary()
  , function :: childFun(State,Block,Attribute,User)
  , state :: State
  , primary_ns :: namespace(Block,Attribute,User)
  , secondary_ns :: dict(binary(), namespace(Block,Attribute,User))
  , parent :: local_block(Block,Attribute,User)
  }.

-type dict(_K,_V) :: dict().
-record(?MODULE,{global,local}).
-define(state,#?MODULE).

-opaque state(Block,Attribute,User,Local) :: ?state{global::global(Block,Attribute,User),local::Local}.
-type state(Block,Attribute,User) :: state(Block,Attribute,User,local_attr(Block,Attribute,User) | local_attrs(Block,Attribute,User) | local_attrval(Block,Attribute,User) | local_block(Block,Attribute,User)).
-define(xml_nsuri,<<"http://www.w3.org/XML/1998/namespace">>).

-spec xml(input()) -> {[child()],state(child(),attribute(),undefined,undefined)}.
xml(Input) -> parser(Input,newGlobalState()).
-spec xml(input(),User) -> {[child()],state(child(),attribute(),User,undefined)}.
xml(Input,User) -> parser(Input,newRootState(User)).

-spec parser(input(),state(Block,Attribute,User,any())) -> {[Block|value()],state(Block,Attribute,User,undefined)}.
parser(Input,State0=?state{}) ->
  State1 = State0?state{local=#local_block{tag='#ROOT#',function = fun (Child,Accum,State) -> {[Child|Accum],State} end, state=[], secondary_ns = dict:new()}},
  State2 = xmlns(none,State1),
  State3 = xmlns(<<"xml">>,?xml_nsuri,State2),
  case run_parser(Input,State3) of
    {ok,State4=?state{local=#local_block{tag='#ROOT#',state=Res}}} ->
      {lists:reverse(Res),State4?state{local=undefined}};
    {ok,State4} -> {error,State4};
    Error -> Error
  end.

-spec newGlobalState() -> state(block(),attribute(),undefined,undefined).
newGlobalState() -> newGlobalState(undefined).
-spec newGlobalState(User) -> state(block(),attribute(),User,undefined).
newGlobalState(UserState) -> newGlobalState(UserState,fun (_Application,GlobalState) -> GlobalState end, fun (_Meta,GlobalState) -> GlobalState end).
-spec newGlobalState(User, metaFun(Block,Attribute,User), applicationFun(Block,Attribute,User)) -> state(Block,Attribute,User,undefined).
newGlobalState(UserState,Application,Meta) ->
  GlobalState1 = #global{entities=dict:new(),intern=dict:new(),namespaces=dict:new(),application=Application,meta=Meta,sections=0,userstate=UserState},
  ?state{global=GlobalState2} = entity(<<"nbsp">>,<<" ">>,
                                entity(<<"lt">>,<<"<">>,
                                entity(<<"gt">>,<<">">>,
                                entity(<<"amp">>,<<"&">>,
                                entity(<<"quot">>,<<"\"">>,
                                namespace(?xml_nsuri,newNamespace(<<"xml">>,?xml_nsuri),
                                namespace(none,newNamespace(none,none),?state{global=GlobalState1}))))))),
  GlobalState2.

newRootState(GlobalState) ->
  ParserState = ?state{global=GlobalState,local=#local_block{
      tag={'#ROOT'},
      function=fun (close,Accum,PS) -> {lists:reverse(Accum),PS};
                   (Child,Accum,PS) -> {[Child|Accum],PS}
               end,
      state=[],
      secondary_ns = dict:new(),
      parent={'#ROOT'}
    }},
  xmlns(<<"xml">>,?xml_nsuri,xmlns(none,ParserState)).


-spec userstate(state(Block,Attribute,User)) -> User when Block :: block(), Attribute :: attribute().
-spec userstate(User,state(Block,Attribute,any())) -> state(Block,Attribute,User) when Block :: block(), Attribute :: attribute().
userstate(?state{global=#global{userstate=UserState}}) -> UserState.
userstate(UserState,ParserState=?state{global=GlobalState}) ->
  ParserState?state{global=GlobalState#global{userstate=UserState}}.

-spec namespace(nsuri(),Parser) -> {namespace(Block,Attribute,User),Parser} when Parser :: state(Block,Attribute,User).
-spec namespace(nsuri(),namespace(Block,Attribute,Global),state(Block,Attribute,Global)) -> state(Block,Attribute,Global) when Block :: block(), Attribute :: attribute().
namespace(URI,ParserState=?state{global=#global{namespaces=Namespaces}}) ->
  case dict:find(URI,Namespaces) of
    {ok,Namespace} -> {Namespace,ParserState};
    error ->
      {Namespace0,ParserState1} = namespace(none,ParserState),
      Namespace = Namespace0#namespace{uri=URI},
      {Namespace,namespace(URI,Namespace,ParserState1)}
  end.
namespace(URI,Namespace,ParserState=?state{global=GlobalState=#global{namespaces=Namespaces}}) ->
      ParserState?state{global=GlobalState#global{namespaces=dict:store(URI,Namespace,Namespaces)}}.

-spec newNamespace(binary()|none,nsuri()) -> namespace(block(),attribute(),_).
-spec newNamespace(binary()|none,nsuri(), blockFuns(Block,Attribute,User)) -> namespace(Block,Attribute,User) when Block :: block(), Attribute :: attribute().
-spec newNamespace(binary()|none,nsuri(), attrFun(Block,Attribute,User), blockFuns(Block,Attribute,User)) -> namespace(Block,Attribute,User) when Block :: block(), Attribute :: attribute().
newNamespace(Prefix,URI) ->
  newNamespace(Prefix,URI,fun block/4).
newNamespace(Prefix,URI,BlockFun) ->
  newNamespace(Prefix,URI,fun attribute/4,BlockFun).
newNamespace(Prefix,URI,AttrFun,BlockFun) ->
  #namespace{uri=URI,prefix=Prefix,attribute=AttrFun,block=BlockFun}.

-spec primaryNamespace(state(Block,Attribute,User)) -> namespace(Block,Attribute,User).
-spec primaryNamespace(namespace(Block,Attribute,User),State) -> State when State :: state(Block,Attribute,User).
primaryNamespace(?state{local=#local_attrs{primary_ns=Namespace}}) -> Namespace;
primaryNamespace(?state{local=#local_block{primary_ns=Namespace}}) -> Namespace.
primaryNamespace(Namespace,ParserState=?state{local=LocalState=#local_attrs{}}) ->
  ParserState?state{local=LocalState#local_attrs{primary_ns=Namespace}};
primaryNamespace(Namespace,ParserState=?state{local=LocalState=#local_block{}}) ->
  ParserState?state{local=LocalState#local_block{primary_ns=Namespace}}.

-spec secondaryNamespace(binary(),state(Block,Attribute,Global)) -> namespace(NBlock,NAttribute,Global) when NBlock :: Block, NAttribute :: Attribute.
-spec secondaryNamespace(binary(),namespace(Block,Attribute,Global),state(NBlock,NAttribute,Global)) -> state(Block,Attribute,Global) when NBlock :: Block, NAttribute :: Attribute.
-spec secondaryNamespaces(state(Block,Attribute,Global)) -> dict(binary(),namespace(NBlock,NAttribute,Global)) when NBlock :: Block, NAttribute :: Attribute.
secondaryNamespace(Prefix,ParserState) -> dict:fetch(Prefix,secondaryNamespaces(ParserState)).
secondaryNamespace(Prefix,Namespace,ParserState) ->
  secondaryNamespaces(dict:store(Prefix,Namespace,secondaryNamespaces(ParserState)),ParserState).
secondaryNamespaces(?state{local=#local_attrs{secondary_ns=Namespaces}}) -> Namespaces;
secondaryNamespaces(?state{local=#local_block{secondary_ns=Namespaces}}) -> Namespaces.
secondaryNamespaces(Namespaces,ParserState=?state{local=LocalState=#local_attrs{}}) -> ParserState?state{local=LocalState#local_attrs{secondary_ns=Namespaces}};
secondaryNamespaces(Namespaces,ParserState=?state{local=LocalState=#local_block{}}) -> ParserState?state{local=LocalState#local_block{secondary_ns=Namespaces}}.

-spec dict:fetch(K,dict(K,V)) -> V.
-spec dict:find(K,dict(K,V)) -> {ok,V} | error.
-spec dict:store(K,V,dict(K,V)) -> dict(K,V).
-spec dict:from_list([{K,V}]) -> dict(K,V).
-spec dict:new() -> dict(any(),any()).

% -spec entity(binary()|{'%',binary()},state(UserState)) -> term().
-spec entity(binary()|{'%',binary()},Parser) -> {value(),Parser} when Parser :: state(any(),any(),any()).
-spec entity(binary()|{'%',binary()},value(),Parser) -> Parser when Parser :: state(any(),any(),any()).
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

-spec intern(input(),Parser) -> {binary(),Parser} when Parser :: state(any(),any(),any()).
-spec name(input(),Parser) -> {tag(),Parser} when Parser :: state(any(),any(),any()).
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
        

-spec addChild(Block,Parser) -> Parser when Parser :: state(Block,any(),any(),local_block(Block,any(),any())).
addChild(Child,ParserState=?state{local=#local_block{function=Fun,state=UserState}}) ->
  {NewUserState,NewParserState} = Fun(Child,UserState,ParserState),
  NewParserState?state{local=NewParserState?state.local#local_block{state=NewUserState}}.

-spec addAttribute(nstag(),value(),Parser) -> Parser when Parser :: state(any(),any(),any(),local_attrs(any(),any(),any())).
addAttribute(xmlns,Value,ParserState) ->
  % io:format("addAttribute(xmlns,~p,...)~n",[Value]),
  xmlns(Value,ParserState);
addAttribute({<<"xmlns">>,Prefix_},Value,ParserState) ->
  % io:format("addAttribute({xmlns,~p},~p,...)~n",[Prefix_,Value]),
  if is_atom(Prefix_) -> Prefix=atom_to_binary(Prefix_,utf8);
     is_binary(Prefix_) -> Prefix = Prefix_
  end,
  xmlns(Prefix,Value,ParserState);
addAttribute(Name,Value,ParserState=?state{local=(LocalState=#local_attrs{attributes=Attributes})}) ->
  % io:format("addAttribute(~p,~p,...)~n",[Name,Value]),
  ParserState?state{local=LocalState#local_attrs{attributes=[{Name,Value}|Attributes]}}.

% -spec attribute(nsuri(),tag(),iolist(),state(UserState,Local)) -> {{nstag(),iolist()}|skip,state(UserState,Local)}.
% attribute(none,Name,Value,ParserState) ->
  % {{Name,Value},ParserState};
% attribute(Uri,Name,Value,ParserState) ->
  % {{{Uri,Name},Value},ParserState}.

-spec block(nsuri(),tag(),[Attribute],[Block|value()]|none,Parser) -> {{nstag(),[Attribute],[Block|value()]|none},Parser}
        when Parser :: state(Block,Attribute,any()),
             is_subtype({nstag(),[Attribute],[Block|value()]|none},Block).

-spec block(nsuri(),tag(),[Attribute],Parser) -> block_children_({nstag(),[Attribute],[Block|value()]},{nstag(),[Attribute],[Block|value()]},Parser)
        when Parser :: state(Block,Attribute,any()),
             is_subtype({nstag(),[Attribute],[Block|value()]|none},Block).

-spec children(none,{nstag(),[Attribute],[]},Parser) -> {{nstag(),[Attribute],none},Parser}
        ;     (close,{nstag(),[Attribute],[Block|value()]},Parser) -> {{nstag(),[Attribute],[Block|value()]},Parser}
        ;     (Block|value(),{nstag(),[Attribute],[Block|value()]},Parser) -> {{nstag(),[Attribute],[Block|value()]},Parser}.
block(none,Tag,Attributes,Children,ParserState) ->
  {{Tag,Attributes,Children},ParserState};
block(Uri,Tag,Attributes,Children,ParserState) ->
  {{{Uri,Tag},Attributes,Children},ParserState}.
block(none,Tag,Attributes,ParserState) ->
  {children,fun children/3,{Tag,Attributes,[]},ParserState};
block(Uri,Tag,Attributes,ParserState) ->
  {children,fun children/3,{{Uri,Tag},Attributes,[]},ParserState}.
children(none,{Tag,Attributes,[]},ParserState) ->
  {{Tag,Attributes,none},ParserState};
children(close,{Tag,Attributes,Children},ParserState) ->
  {{Tag,Attributes,lists:reverse(Children)},ParserState};
children(Child,{Tag,Attributes,Children},ParserState) ->
  {{Tag,Attributes,[Child,Children]},ParserState}.

-spec attribute(nsuri(),tag(),value(),Parser) -> {{nstag(),value()},Parser} when Parser :: state(any(),Attribute,any()), is_subtype({nstag(),value()},Attribute).
attribute(none,Name,Value,ParserState) -> {{Name,Value},ParserState};
attribute(NS,Name,Value,ParserState) -> {{{NS,Name},Value},ParserState}.

-spec xmlns(tag(),value(),Parser) -> Parser when Parser :: state(any(),any(),any()).
xmlns(Name,Value,ParserState) when is_atom(Name) ->
  {BName,ParserState2} = intern(atom_to_binary(Name,utf8),ParserState),
  xmlns(BName,Value,ParserState2);
xmlns(Name,none,ParserState) ->
  {Namespace,ParserState2} = namespace(none,ParserState),
  secondaryNamespace(Name,Namespace,ParserState2);
xmlns(Name,Value,ParserState) ->
  {Namespace,ParserState2} = namespace(iolist_to_binary(Value),ParserState),
  secondaryNamespace(Name,Namespace,ParserState2).
-spec xmlns(value(),Parser) -> Parser when Parser :: state(any(),any(),any()).
xmlns(none,ParserState) ->
  {Namespace,ParserState2} = namespace(none,ParserState),
  primaryNamespace(Namespace,ParserState2);
xmlns(Value,ParserState) ->
  {Namespace,ParserState2} = namespace(iolist_to_binary(Value),ParserState),
  primaryNamespace(Namespace,ParserState2).

-spec nsify(input(),Parser) -> {nstag(),Parser} when Parser :: state(any(),any(),any()).
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
  {ParserState2?state{local=#local_attrs{tag=Tag,attributes=[],?parent(LocalState)}},Input}.

?event_end_block(CTag_,{ParserState1,Input}) ->
  % io:format("CTag = ~p~n",[CTag_]),
  {CTag,ParserState2=?state{local=#local_block{tag=Tag,function=Fun,state=UserState}}} = nsify(CTag_,ParserState1),
  {Block,ParserState3} = Fun(close,UserState,ParserState2),
  ParserState4 = addChild(Block,ParserState3?state{local=ParserState3?state.local#local_block.parent}),
  case CTag of
    Tag -> {ParserState4,Input};
    _ ->
      ?event_end_block(CTag,{ParserState4,Input})
  end.

?event_attribute_name(Name_,{ParserState1,Input}) ->
  {Name,ParserState2=?state{local=Parent}} = nsify(Name_,ParserState1),
  {ParserState2?state{local=#local_attr{name=Name,parent=Parent}},Input}.

preclose_tag(ParserState1=?state{local=#local_attrs{tag=Tag,attributes=Attrs_}}) ->
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
    {children,CFun,State,ParserState3=?state{local=#local_attrs{parent=Parent,primary_ns=Primary,secondary_ns=Secondary}}} ->
      {ParserState3?state{local=#local_block{tag=Tag,function=CFun,state=State,parent=Parent,primary_ns=Primary,secondary_ns=Secondary}},Input}
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
          {skip,ParserState3=?state{local=#local_attrs{parent=Parent}}} -> {ParserState3?state{local=Parent},Input};
          {blob,BFun,State,ParserState3} ->
            % io:format("close/tag[4]"),
            {Node,ParserState4=?state{local=#local_attrs{parent=Parent}}} = BFun(none,State,ParserState3),
            % io:format("close/tag[5]"),
            {addChild(Node,ParserState4?state{local=Parent}),Input};
          {children,CFun,State,ParserState3} ->
            % io:format("close/tag[6]"),
            {Node,ParserState4=?state{local=#local_attrs{parent=Parent}}} = CFun(close,State,ParserState3),
            % io:format("close/tag[7]"),
            {addChild(Node,ParserState4?state{local=Parent}),Input}
        end;
     is_function(Fun,5) ->
        % io:format("close/tag[8]"),
        {Node,ParserState3=?state{local=#local_attrs{parent=Parent}}} = Fun(Uri,PTag,Attrs,none,ParserState2),
        % io:format("close/tag[9]"),
        {addChild(Node,ParserState3?state{local=Parent}),Input}
  end.

?event_attribute_begin_value({ParserState=?state{local=#local_attr{name=Name,parent=Parent}},Input}) ->
  {ParserState?state{local=#local_attrval{name=Name,value=[],parent=Parent}},Input}.
?event_attribute_value(Value,{ParserState=?state{local=#local_attr{name=Name,parent=Parent}},Input}) ->
  {addAttribute(Name,Value,ParserState?state{local=Parent}),Input}.

?event_attribute_end_value({ParserState=?state{local=#local_attrval{name=Name,value=Value,parent=Parent}},Input}) ->
  {addAttribute(Name,lists:reverse(Value),ParserState?state{local=Parent}),Input}.
?event_attribute_value_text(Text,{ParserState=?state{local=Local=#local_attrval{value=Value}},Input}) ->
  {ParserState?state{local=Local#local_attrval{value=[Text|Value]}},Input}.
?event_attribute_value_entity(Name,{ParserState1,Input}) ->
  {Entity,ParserState2=?state{local=Local=#local_attrval{value=Value}}} = entity(Name,ParserState1),
  {ParserState2?state{local=Local#local_attrval{value=[Entity|Value]}},Input}.

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
ignore_sections_(Input,N) -> ?parse_input(Input);
ignore_sections_(<<_,"]]>",Input>>,N) ->
  ignore_sections(Input,N-1);
ignore_sections_(<<_,"<![",Input>>,N) ->
  ignore_sections(Input,N+1).

scan_section(Input) -> ?parse_input(Input);
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

  
  


