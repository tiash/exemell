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

-export([new/0,new/1,new/2]).
-export([xml/1,xml/2]).
-export([userstate/1,userstate/2]).
-export([namespace/2,namespace/3]).
-export([xmlns/2,xmlns/3]).
-export([addChild/2]).

% -compile([inline,native,{hipe,[o3]}]).
% -compile([native,{hipe,[o3]}]).
-compile([inline]).
% -compile([bin_opt_info]).

-define(entrypoint,run_parser).

-include("exemell.hrl").


-record(global,
  { entities = dict:new() :: dict(binary() | {'%',binary()},term())
  , intern = dict:new() :: dict(binary(),binary())
  , namespaces = dict:new() :: dict(nsuri(),namespace())
  , module = exemell_parser :: module()
  , sections = 0:: non_neg_integer()
  , userstate = undefined :: any()
  }).

-define(parent(Parent), primary_ns = primaryNamespace(?state{local=Parent}), secondary_ns = secondaryNamespaces(?state{local=Parent}), parent=Parent).

-record(local_block,
  { tag :: binary()
  , module :: module()
  , state :: any()
  , primary_ns :: namespace()
  , secondary_ns :: dict(binary(), namespace())
  , parent :: #local_block{}
  }).
      
-record(local_attrs,
  { tag :: binary()
  , attributes :: [{nstag(), value()}]
  , primary_ns :: namespace()
  , secondary_ns :: dict(binary(), namespace())
  , parent :: #local_block{}
  }).

-record(local_attr,
  { name :: binary()
  , parent :: #local_attrs{}
  }).

-record(local_attrval,{name :: binary(), value :: value(), parent :: #local_attrs{}}).

-type dict(_K,_V) :: dict().
-record(?MODULE,{global :: #global{}, local :: #local_attr{} | #local_attrs{} | #local_attrval{} | #local_block{}}).
-define(state,#?MODULE).

% -opaque state(Local) :: ?state{local::Local}.
-export_type([state/0]).
-opaque state() :: ?state{}. % ?state{local::#local_block{}}.



-spec xml(input()) -> {ok,[child()],exemell:state()} | {error,_}.
xml(Input) -> xml(Input,new()).
-spec xml(input(),Parser) -> {ok,[child()],Parser} | {error,_} when Parser :: exemell:state().
xml(Input,State0=?state{}) when is_binary(Input) ->
  case run_parser(Input,reset(State0)) of
    {ok,State1=?state{local=#local_block{tag=undefined,state={undefined,[],Res}}}} ->
      {ok,lists:reverse(Res),State1?state{local=undefined}};
    {ok,State1} -> {error,State1};
    Error -> Error
  end;
xml(Input,State0=?state{}) -> xml(iolist_to_binary(Input),State0).

-spec new() -> exemell:state().
new() -> new(undefined).
-spec new(term()) -> exemell:state().
new(User) -> new(User,exemell_parser).
-spec new(term(), module()) -> exemell:state().
new(User,Module) ->
  code:ensure_loaded(exemell_namespace_xml),
  code:ensure_loaded(exemell_namespace),
  GlobalState1 = ?state{global=#global{entities=dict:new(),intern=dict:new(),namespaces=exemell_ns_reg:get(),module=Module,userstate=User}},
  GlobalState2 = entity(<<"nbsp">>,<<" ">>,GlobalState1),
  GlobalState3 = entity(<<"lt">>,<<"<">>,GlobalState2),
  GlobalState4 = entity(<<"gt">>,<<">">>,GlobalState3),
  GlobalState5 = entity(<<"amp">>,<<"&">>,GlobalState4),
  GlobalState6 = entity(<<"quot">>,<<"\"">>,GlobalState5),
  {_,GlobalState7} = namespace(exemell_namespace_xml,GlobalState6),
  {_,GlobalState8} = namespace(exemell_namespace,GlobalState7),
  GlobalState8.

reset(Parser0) ->
  Parser1 = Parser0?state{local=#local_block{
      tag=undefined,
      module=exemell_block,
      state={undefined,[],[]},
      secondary_ns = dict:new()
    }},
  xmlns(<<"xml">>,exemell_namespace_xml,xmlns(exemell_namespace,Parser1)).

-spec userstate(Parser) -> term()
        when Parser :: exemell:state().
-spec userstate(term(),Parser) -> Parser
        when Parser :: exemell:state().
userstate(?state{global=#global{userstate=UserState}}) -> UserState.
userstate(UserState,ParserState=?state{global=GlobalState}) ->
  ParserState?state{global=GlobalState#global{userstate=UserState}}.

-spec namespace(nsuri() | namespace(),Parser) -> {namespace(),Parser} when Parser :: exemell:state().
-spec namespace(nsuri(), namespace(),Parser) -> Parser when Parser :: exemell:state().
namespace(URI,ParserState=?state{global=#global{namespaces=Namespaces}}) when is_binary(URI); URI=:=none ->
  case dict:find(URI,Namespaces) of
    {ok,Namespace} -> {Namespace,ParserState};
    error ->
      Namespace = exemell_namespace:new(URI),
      {Namespace,namespace(URI,Namespace,ParserState)}
  end;
namespace(Namespace,ParserState) -> {Namespace,namespace(Namespace:xmlns(),Namespace,ParserState)}.
namespace(URI,Namespace,ParserState=?state{global=GlobalState=#global{namespaces=Namespaces}}) ->
      ParserState?state{global=GlobalState#global{namespaces=dict:store(URI,Namespace,Namespaces)}}.

-spec primaryNamespace(Parser) -> namespace()
        when Parser :: exemell:state().
-spec primaryNamespace(namespace(),Parser) -> Parser
        when Parser :: exemell:state().
primaryNamespace(?state{local=#local_attrs{primary_ns=Namespace}}) -> Namespace;
primaryNamespace(?state{local=#local_block{primary_ns=Namespace}}) -> Namespace.
primaryNamespace(Namespace,ParserState=?state{local=LocalState=#local_attrs{}}) ->
  ParserState?state{local=LocalState#local_attrs{primary_ns=Namespace}};
primaryNamespace(Namespace,ParserState=?state{local=LocalState=#local_block{}}) ->
  ParserState?state{local=LocalState#local_block{primary_ns=Namespace}}.

-spec secondaryNamespace(binary(),Parser) -> namespace()
        when Parser :: exemell:state().
-spec secondaryNamespace(binary(),namespace(),Parser) -> Parser
        when Parser :: exemell:state().
-spec secondaryNamespaces(Parser) -> dict(binary(),namespace())
        when Parser :: exemell:state().
-spec secondaryNamespaces(dict(binary(),namespace()),Parser) -> Parser
        when Parser :: exemell:state().
secondaryNamespace(Prefix,ParserState) -> dict:fetch(Prefix,secondaryNamespaces(ParserState)).
secondaryNamespace(Prefix,Namespace,ParserState) ->
  secondaryNamespaces(dict:store(Prefix,Namespace,secondaryNamespaces(ParserState)),ParserState).
secondaryNamespaces(?state{local=#local_attrs{secondary_ns=Namespaces}}) -> Namespaces;
secondaryNamespaces(?state{local=#local_block{secondary_ns=Namespaces}}) -> Namespaces.
secondaryNamespaces(Namespaces,ParserState=?state{local=LocalState=#local_attrs{}}) -> ParserState?state{local=LocalState#local_attrs{secondary_ns=Namespaces}};
secondaryNamespaces(Namespaces,ParserState=?state{local=LocalState=#local_block{}}) -> ParserState?state{local=LocalState#local_block{secondary_ns=Namespaces}}.

% -spec dict:fetch(K,dict(K,V)) -> V.
% -spec dict:find(K,dict(K,V)) -> {ok,V} | error.
% -spec dict:store(K,V,dict(K,V)) -> dict(K,V).
% -spec dict:from_list([{K,V}]) -> dict(K,V).
% -spec dict:new() -> dict(any(),any()).

% -spec entity(binary()|{'%',binary()},state(UserState)) -> term().
-spec entity(binary()|{'%',binary()},Parser) -> {value(),Parser}
        when Parser :: exemell:state().
-spec entity(binary()|{'%',binary()},value(),Parser) -> Parser
        when Parser :: exemell:state().
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

-spec intern(input(),Parser) -> {binary(),Parser}
        when Parser :: exemell:state().
-spec name(input(),Parser) -> {tag(),Parser}
        when Parser :: exemell:state().
% intern(Value,ParserState) -> {Value,ParserState}.
intern(Value,ParserState=?state{global=Global=#global{intern=Interned}}) when is_binary(Value) ->
  case dict:find(Value,Interned) of
    {ok,IValue} -> {IValue,ParserState};
    error ->
      IValue = binary:copy(Value), %% Check if Value is a subbinary
      {IValue,ParserState?state{global=Global#global{intern=dict:store(IValue,IValue,Interned)}}}
  end;
intern(Value,ParserState) -> intern(iolist_to_binary(Value),ParserState).
% name(Name,ParserState) when is_atom(Name) -> {Name,ParserState};
name(Name,ParserState) when is_binary(Name) -> 
  try {binary_to_existing_atom(Name,utf8),ParserState}
  catch
    error:badarg -> intern(Name,ParserState)
  end
%  ;
% name(Name,ParserState) -> name(iolist_to_binary(Name),ParserState)
  .
        

-spec addChild(child(),Parser) -> Parser
        when Parser :: exemell:state().
addChild(Child,ParserState=?state{local=#local_block{module=Module,state=UserState}}) ->
  {NewUserState,NewParserState} = Module:xml_child(Child,UserState,ParserState),
  NewParserState?state{local=NewParserState?state.local#local_block{state=NewUserState}}.

-spec addAttribute_(input(),value(),Parser) -> Parser
        when Parser :: exemell:state().
addAttribute_(Name_,Value,Parser0) ->
  {Name,Parser1} = nsify(Name_,Parser0),
  addAttribute(Name,Value,Parser1).

-spec addAttribute(nstag(),value(),Parser) -> Parser
        when Parser :: exemell:state().
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

-spec xmlns(tag(),value(),Parser) -> Parser
        when Parser :: exemell:state().
xmlns(Name,Value,ParserState) when is_atom(Name) ->
  {BName,ParserState2} = intern(atom_to_binary(Name,utf8),ParserState),
  xmlns(BName,Value,ParserState2);
xmlns(Name,NS,ParserState) ->
  {Namespace,ParserState2} = namespace(if is_atom(NS); is_tuple(NS) -> NS; true -> iolist_to_binary(NS) end,ParserState),
  secondaryNamespace(Name,Namespace,ParserState2).
-spec xmlns(value(),Parser) -> Parser
        when Parser :: exemell:state().
xmlns(NS,ParserState) ->
  {Namespace,ParserState2} = namespace(if is_atom(NS); is_tuple(NS) -> NS; true -> iolist_to_binary(NS) end,ParserState),
  primaryNamespace(Namespace,ParserState2).

-spec nsify(input(),Parser) -> {nstag(),Parser}
        when Parser :: exemell:state().
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

event_text(<<>>,{ParserState,Input}) ->
  {ParserState,Input};
event_text(Text,{ParserState,Input}) ->
  {addChild(Text,ParserState),Input}.
event_entity(Name,{ParserState1,Input}) ->
  {Entity,ParserState2} = entity(Name,ParserState1),
  {addChild(Entity,ParserState2),Input}.
event_comment(_Comment,{ParserState,Input}) ->
  {ParserState,Input}.
event_application(Application,{ParserState=?state{global=#global{module=Module}},Input}) ->
  {Module:xml_application(Application,ParserState),Input}.
event_meta(Meta,{ParserState=?state{global=#global{module=Module}},Input}) ->
  {Module:xml_meta(Meta,ParserState),Input}.
event_open_tag(RTag,{ParserState1=?state{local=LocalState},Input}) ->
  % io:format("OTag=~p~n",[RTag]),
  % {Tag,ParserState2=?state{local=LocalState}} = nsify(RTag,ParserState1),
  {ParserState1?state{local=#local_attrs{tag=RTag,attributes=[],?parent(LocalState)}},Input}.

event_end_block(CTag,{ParserState1=?state{local=#local_block{tag=Tag,module=Module,state=State}},Input}) ->
  % io:format("CTag = ~p~n",[CTag_]),
  % {CTag,ParserState2=?state{local=#local_block{tag=Tag,module=Module,state=UserState}}} = nsify(CTag_,ParserState1),
  {Block,ParserState2} = Module:xml_end(State,ParserState1),
  ParserState4 = addChild(Block,ParserState2?state{local=ParserState2?state.local#local_block.parent}),
  case CTag of
    Tag -> {ParserState4,Input};
    _ ->
      event_end_block(CTag,{ParserState4,Input})
  end.

event_attribute_name(Name_,{ParserState1,Input}) ->
  {Name,ParserState2=?state{local=Parent}} = nsify(Name_,ParserState1),
  {ParserState2?state{local=#local_attr{name=Name,parent=Parent}},Input}.

preclose_tag(ParserState1=?state{local=#local_attrs{tag=RTag,attributes=Attrs_}}) ->
  {Attrs,ParserState2} = process_attrs(lists:reverse(Attrs_),ParserState1,[]),
  {Tag,ParserState3} = nsify(RTag,ParserState2),
  {RTag,Tag,Attrs,ParserState3}.

process_attrs([],ParserState,Accum) -> {lists:reverse(Accum),ParserState};
process_attrs([{Name,Value}|Attrs],ParserState1,Accum) ->
  case Name of
    {Prefix,PName} ->
      Namespace = secondaryNamespace(Prefix,ParserState1),
      Uri = Namespace:xmlns();
    PName ->  
      Namespace = primaryNamespace(ParserState1),
      Uri = none
  end,
  case Namespace:xml_attribute(Uri,PName,Value,ParserState1) of
    {skip,ParserState2} -> process_attrs(Attrs,ParserState2,Accum);
    {Attr,ParserState2} -> process_attrs(Attrs,ParserState2,[Attr|Accum])
  end.
  
  

event_close_tag_begin_block({ParserState1,Input}) ->
  % io:format("event_close_tag_begin_block... RTag = ~p~n",[RTag]),
  {RTag,Tag,Attrs,ParserState2} = preclose_tag(ParserState1),
  % io:format("tag/enter[1] ~p~n",[ParserState2]),
  % io:format("tag/enter[1].tag ~p~n",[Tag]),
  % io:format("tag/enter[1].nss ~p~n",[dict:to_list(secondaryNamespaces(ParserState2))]),
  % io:format("tag/enter[1].attrs ~p~n",[Attrs]),
  case Tag of
    {Prefix,PTag} ->
      Namespace = secondaryNamespace(Prefix,ParserState2);
    PTag -> 
      Namespace = primaryNamespace(ParserState2)
  end,
  Uri = Namespace:xmlns(),
  case Namespace:xml_block(Uri,PTag,Attrs,ParserState2) of
    {skip,ParserState3} -> erlang:exit(todo), {ParserState3,Input};
    {blob,CModule,State,ParserState3=?state{local=_Parent}} ->
      {Body,After} = fast_scan(Input,0,Input,RTag,1),
      {Node,ParserState4=?state{local=#local_attrs{parent=Parent}}} = CModule:xml_blob(Body,State,ParserState3),
      % io:format("close/tag[7]"),
      {addChild(Node,ParserState4?state{local=Parent}),After};
    {children,CModule,State,ParserState3=?state{local=#local_attrs{parent=Parent,primary_ns=Primary,secondary_ns=Secondary}}} ->
      {ParserState3?state{local=#local_block{tag=RTag,module=CModule,state=State,parent=Parent,primary_ns=Primary,secondary_ns=Secondary}},Input}
  end.

fast_scan(Input,Offs,<<"<[CDATA[",After/bytes>>,Tag,N) ->
  fast_scan_cdata(Input,Offs+8,After,Tag,N);
fast_scan(Input,Offs,<<"<[",After/bytes>>,Tag,N) ->
  fast_scan_section(Input,Offs+2,After,Tag,N);
fast_scan(Input,Offs,<<"]]>",After/bytes>>,Tag,N) ->
  fast_scan(Input,Offs+3,After,Tag,N);
fast_scan(Input,Offs,<<"</",After/bytes>>,Tag,N) ->
  fast_scan_ctag(Input,Offs,Offs+2,After,Tag,N);
fast_scan(Input,Offs,<<"<!--",After/bytes>>,Tag,N) ->
  fast_scan_comment(Input,Offs+4,After,Tag,N);
fast_scan(Input,Offs,<<"<!",After/bytes>>,Tag,N) ->
  fast_scan_meta(Input,Offs+2,After,Tag,N);
fast_scan(Input,Offs,<<"<%",After/bytes>>,Tag,N) ->
  fast_scan_app(Input,Offs+2,After,Tag,N);
fast_scan(Input,Offs,<<"<",After/bytes>>,Tag,N) ->
  fast_scan_tag(Input,Offs+1,After,Tag,N);
fast_scan(Input,Offs,<<_,After/bytes>>,Tag,N) ->
  fast_scan(Input,Offs+1,After,Tag,N).
fast_scan_cdata(Input,Offs,<<"]]>",After/bytes>>,Tag,N) ->
  fast_scan(Input,Offs+3,After,Tag,N);
fast_scan_cdata(Input,Offs,<<_,After/bytes>>,Tag,N) ->
  fast_scan_cdata(Input,Offs+1,After,Tag,N).
fast_scan_section(Input,Offs,<<"[",After/bytes>>,Tag,N) ->
  fast_scan_section(Input,Offs+1,After,Tag,N);
fast_scan_section(Input,Offs,<<_,After/bytes>>,Tag,N) ->
  fast_scan_section(Input,Offs+1,After,Tag,N).
fast_scan_comment(Input,Offs,<<"-->",After/bytes>>,Tag,N) ->
  fast_scan(Input,Offs+3,After,Tag,N);
fast_scan_comment(Input,Offs,<<_,After/bytes>>,Tag,N) ->
  fast_scan_comment(Input,Offs+1,After,Tag,N).
fast_scan_meta(Input,Offs,<<">",After/bytes>>,Tag,N) ->
  fast_scan(Input,Offs+3,After,Tag,N);
fast_scan_meta(Input,Offs,<<_,After/bytes>>,Tag,N) ->
  fast_scan_meta(Input,Offs+1,After,Tag,N).
fast_scan_app(Input,Offs,<<"%>",After/bytes>>,Tag,N) ->
  fast_scan(Input,Offs+3,After,Tag,N);
fast_scan_app(Input,Offs,<<_,After/bytes>>,Tag,N) ->
  fast_scan_app(Input,Offs+1,After,Tag,N).
fast_scan_tag(Input,Offs,<<" ",After/bytes>>,Tag,N) ->
  fast_scan_tag(Input,Offs+1,After,Tag,N);
fast_scan_tag(Input,Offs,<<"\t",After/bytes>>,Tag,N) ->
  fast_scan_tag(Input,Offs+1,After,Tag,N);
fast_scan_tag(Input,Offs,<<"\r",After/bytes>>,Tag,N) ->
  fast_scan_tag(Input,Offs+1,After,Tag,N);
fast_scan_tag(Input,Offs,<<"\n",After/bytes>>,Tag,N) ->
  fast_scan_tag(Input,Offs+1,After,Tag,N);
fast_scan_tag(Input,Offs,After,Tag,N) ->
  TS = byte_size(Tag),
  case After of
    <<Tag:TS/bytes,After2/bytes>> ->
      case After2 of
        <<">",Continue/bytes>> -> fast_scan(Input,Offs+byte_size(Tag)+1,Continue,Tag,N+1);
        <<"/>",Continue/bytes>> -> fast_scan(Input,Offs+byte_size(Tag)+2,Continue,Tag,N);
        <<" ",Continue/bytes>> -> fast_scan_etag(Input,Offs+byte_size(Tag)+1,Continue,Tag,N+1,N);
        <<"\t",Continue/bytes>> -> fast_scan_etag(Input,Offs+byte_size(Tag)+1,Continue,Tag,N+1,N);
        <<"\r",Continue/bytes>> -> fast_scan_etag(Input,Offs+byte_size(Tag)+1,Continue,Tag,N+1,N);
        <<"\n",Continue/bytes>> -> fast_scan_etag(Input,Offs+byte_size(Tag)+1,Continue,Tag,N+1,N);
        <<_,Continue/bytes>> -> fast_scan_etag(Input,Offs+byte_size(Tag)+1,Continue,Tag,N,N)
      end;
    _ -> fast_scan_etag(Input,Offs,After,Tag,N,N)
  end.


fast_scan_etag(Input,Offs,<<"/>",Continue/bytes>>,Tag,_,M) ->
  fast_scan(Input,Offs+2,Continue,Tag,M);
fast_scan_etag(Input,Offs,<<">",Continue/bytes>>,Tag,N,_) ->
  fast_scan(Input,Offs+1,Continue,Tag,N);
fast_scan_etag(Input,Offs,<<"\"",Continue/bytes>>,Tag,N,M) ->
  fast_scan_quot(Input,Offs+1,Continue,Tag,N,M);
fast_scan_etag(Input,Offs,<<_,Continue/bytes>>,Tag,N,M) ->
  fast_scan_etag(Input,Offs+1,Continue,Tag,N,M).
fast_scan_quot(Input,Offs,<<"\"",Continue/bytes>>,Tag,N,M) ->
  fast_scan_etag(Input,Offs+1,Continue,Tag,N,M);
fast_scan_quot(Input,Offs,<<_,Continue/bytes>>,Tag,N,M) ->
  fast_scan_quot(Input,Offs+1,Continue,Tag,N,M).
fast_scan_ctag(Input,Offs0,Offs,After,Tag,N) ->
  TS = byte_size(Tag),
  case After of
    <<Tag:TS/bytes,After2/bytes>> ->
      case After2 of
        <<">",Continue/bytes>> ->
          case N of 1 -> fast_scan_finish(Input,Offs0,Continue);
                    _ -> fast_scan(Input,Offs+byte_size(Tag)+1,Continue,Tag,N-1)
          end;
        <<" ",Continue/bytes>> -> fast_scan_cetag(Input,Offs0,Offs+byte_size(Tag)+1,Continue,Tag,N);
        <<"\t",Continue/bytes>> -> fast_scan_cetag(Input,Offs0,Offs+byte_size(Tag)+1,Continue,Tag,N);
        <<"\r",Continue/bytes>> -> fast_scan_cetag(Input,Offs0,Offs+byte_size(Tag)+1,Continue,Tag,N);
        <<"\n",Continue/bytes>> -> fast_scan_cetag(Input,Offs0,Offs+byte_size(Tag)+1,Continue,Tag,N);
        <<_,Continue/bytes>> -> fast_scan_cetag(Input,Offs+byte_size(Tag)+1,Continue,Tag,N)
      end;
    _ -> fast_scan_cetag(Input,Offs,After,Tag,N)
  end.
fast_scan_cetag(Input,Offs0,_Offs,<<">",Continue/bytes>>,_Tag,1) ->
  fast_scan_finish(Input,Offs0,Continue);
fast_scan_cetag(Input,_Offs0,Offs,<<">",Continue/bytes>>,Tag,N) ->
  fast_scan(Input,Offs+1,Continue,Tag,N-1);
fast_scan_cetag(Input,Offs0,Offs,<<_,Continue/bytes>>,Tag,N) ->
  fast_scan_cetag(Input,Offs0,Offs+1,Continue,Tag,N).
fast_scan_cetag(Input,Offs,<<">",Continue/bytes>>,Tag,N) ->
  fast_scan(Input,Offs+1,Continue,Tag,N);
fast_scan_cetag(Input,Offs,<<_,Continue/bytes>>,Tag,N) ->
  fast_scan_cetag(Input,Offs+1,Continue,Tag,N).
fast_scan_finish(Input,Offs,Continue) ->
  % io:format("[~s:~p] fast_scan_finish(~p,~p).~n",[?FILE,?LINE,Input,Offs]),
  {binary_part(Input,0,Offs),Continue}.

  
event_close_tag({ParserState1,Input}) ->
  % io:format("close/tag[1]"),
  {_RTag,Tag,Attrs,ParserState2} = preclose_tag(ParserState1),
  % io:format("close/tag[2]"),
  case Tag of
    {Prefix,PTag} ->
      Namespace = secondaryNamespace(Prefix,ParserState2);
    PTag -> 
      Namespace = primaryNamespace(ParserState2)
  end,
  Uri = Namespace:xmlns(),
  % io:format("close/tag[3]"),
  case Namespace:xml_block(Uri,PTag,Attrs,ParserState2) of
    {skip,ParserState3=?state{local=#local_attrs{parent=Parent}}} -> {ParserState3?state{local=Parent},Input};
    {blob,BModule,State,ParserState3} ->
      % io:format("close/tag[4]"),
      {Node,ParserState4=?state{local=#local_attrs{parent=Parent}}} = BModule:xml_blob(none,State,ParserState3),
      % io:format("close/tag[5]"),
      {addChild(Node,ParserState4?state{local=Parent}),Input};
    {children,CModule,State,ParserState3} ->
      % io:format("close/tag[6]"),
      {Node,ParserState4=?state{local=#local_attrs{parent=Parent}}} = CModule:xml_end(State,ParserState3),
      % io:format("close/tag[7]"),
      {addChild(Node,ParserState4?state{local=Parent}),Input}
  end.

event_attribute_begin_value({ParserState=?state{local=#local_attr{name=Name,parent=Parent}},Input}) ->
  {ParserState?state{local=#local_attrval{name=Name,value=[],parent=Parent}},Input}.
event_attribute_value(Value,{ParserState=?state{local=#local_attr{name=Name,parent=Parent}},Input}) ->
  {addAttribute_(Name,Value,ParserState?state{local=Parent}),Input}.

event_attribute_end_value({ParserState=?state{local=#local_attrval{name=Name,value=Value,parent=Parent}},Input}) ->
  {addAttribute_(Name,lists:reverse(Value),ParserState?state{local=Parent}),Input}.
event_attribute_value_text(Text,{ParserState=?state{local=Local=#local_attrval{value=Value}},Input}) ->
  {ParserState?state{local=Local#local_attrval{value=[Text|Value]}},Input}.
event_attribute_value_entity(Name,{ParserState1,Input}) ->
  {Entity,ParserState2=?state{local=Local=#local_attrval{value=Value}}} = entity(Name,ParserState1),
  {ParserState2?state{local=Local#local_attrval{value=[Entity|Value]}},Input}.

event_end_section({ParserState,Input}) ->
  {decrement_section(ParserState),Input}.
event_begin_section(Name_,{ParserState1,Input1}) ->
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
        event_begin_section(Entity,{ParserState2,Input1});
    _ -> erlang:error({unsupported_sction,Name})
  end.



decrement_section(State=?state{global=Global=#global{sections=N}}) -> State?state{global=Global#global{sections=N-1}}.
increment_section(State=?state{global=Global=#global{sections=N}}) -> State?state{global=Global#global{sections=N+1}}.

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



-include("parser.hrl").



