-module(exemellp).

-include("exemell.hrl").
-ifndef(DEBUG).
-define(DEBUG,true).
-endif.
-include("debug.hrl").

-callback xml(exemellp:state(),_) -> iolist().

-export([escape/1,new/0,attribute/3,attribute/4,name/2,name_/2,namespace/2,secondary/2,primary/2,xml/2,xml/1,xml/4]).

-define(xml_nsuri,<<"http://www.w3.org/XML/1998/namespace">>).

-behaviour(exemellp).

-type dict(_K,_V) :: dict().
%% And now the extern code....
-export_type([state/0]).
-opaque state() :: {namespace(),dict(nsuri(),namespace())}.

-spec new() -> state().
new() -> {none,dict:store(?xml_nsuri,<<"xml">>,dict:new())}.

-spec primary(none|nsuri()|namespace(),state()) -> {iolist(),state()}.
primary(NS,Printer={NS,_}) -> {[],Printer};
primary(URI,Printer) when is_list(URI) -> primary(iolist_to_binary(URI),Printer);
primary(URI,Printer) when is_binary(URI); URI=:=none -> primary(exemell_namespace:new(URI),Printer);
primary(NS,{_,Secondary}) -> {[<<" xmlns=\"">>,escape(NS:xmlns()),$"],{NS,Secondary}}.

-spec secondary(none|nsuri()|namespace(),state()) -> {binary(),iolist(),state()}.
secondary(URI,Printer) when is_list(URI) -> secondary(iolist_to_binary(URI),Printer);
secondary(URI,Printer) when is_binary(URI); URI=:=none -> secondary(exemell_namespace:new(URI),Printer);
secondary(NS,Printer={Primary,Secondary}) ->
  case dict:find(NS,Secondary) of
    {ok,Prefix} -> {Prefix,[],Printer};
    error ->
      case NS:xml_prefix() of
        none -> Prefix = iolist_to_binary(["ns",integer_to_list(dict:size(Secondary)+1)]);
        Prefix -> ok
      end,
      {Prefix,[<<" xmlns:">>,Prefix,$=,$",escape(NS:xmlns()),$"],{Primary,dict:store(NS,Prefix,Secondary)}}
  end.

-spec namespace(none|nsuri()|namespace(),state()) -> {binary(),iolist(),state()} | {iolist(),state()}.
namespace(NS,Printer={NS,_}) -> {[],Printer};
namespace(URI,Printer) when is_list(URI) -> namespace(iolist_to_binary(URI),Printer);
namespace(URI,Printer) when is_binary(URI); URI=:=none -> namespace(exemell_namespace:new(URI),Printer);
namespace(NS,Printer={Primary,Secondary}) ->
  case dict:find(NS,Secondary) of
    {ok,Prefix} -> {Prefix,[],Printer};
    error ->
      case NS:xml_prefix() of
        none -> {[<<" xmlns=\"">>,escape(NS:xmlns()),$"],{NS,Secondary}};
        Prefix -> {[<<" xmlns:">>,Prefix,$=,$",escape(NS:xmlns()),$"],{Primary,dict:store(NS,Prefix,Secondary)}}
      end
  end.

-spec name({binary(),nsuri()}|nsuri(),binary(),state()) -> {iolist(),iolist(),state()}.
-spec name_({binary(),nsuri()}|nsuri(),binary(),state()) -> {iolist(),iolist(),state()}.
name(NS,Tag,P0) when is_atom(Tag) -> name(NS,atom_to_binary(Tag,utf8),P0);
name(NS,Tag,P0) ->
  case namespace(NS,P0) of
    {PRE,IO,P1} -> {IO,[PRE,$:,Tag],P1};
    {IO,P1} -> {IO,Tag,P1}
  end.
name_(NS,Tag,P0) when is_atom(Tag) -> name_(NS,atom_to_binary(Tag,utf8),P0);
name_(none,TAG,P0) ->
  {[],TAG,P0};
name_(NS,Tag,P0) ->
  case secondary(NS,P0) of
    {PRE,IO,P1} -> {IO,[PRE,$:,Tag],P1}
    % {IO,P1} -> {IO,Tag,P1}
  end.
-spec name(binary()|{nsuri(),binary()}|{{binary(),nsuri()},binary()},state()) -> {iolist(),iolist(),state()}.
name({NS,Tag},P0) -> name(NS,Tag,P0);
name(Tag,Printer) -> name(none,Tag,Printer).
name_({NS,Tag},P0) -> name_(NS,Tag,P0);
name_(Tag,Printer) -> name_(none,Tag,Printer).

attribute(NS,Name,Value,P0) ->
  {Pre,IOName,P1} = name_(NS,Name,P0),
  {Pre,[$ ,IOName,$=,$",escape(Value),$"],P1}.
attribute(Name,Value,P0) ->
  {Pre,IOName,P1} = name_(Name,P0),
  {Pre,[$ ,IOName,$=,$",escape(Value),$"],P1}.
  
escape(INPUT) -> escape(INPUT,[]).
escape([],ACCUM) -> ACCUM;
escape([H|T],ACCUM) ->
  escape(T,escape(H,ACCUM));
escape(INPUT,ACCUM) when is_binary(INPUT) ->
  escape(INPUT,0,INPUT,ACCUM);
escape($",ACCUM) -> [ACCUM,<<"&quot;">>];
escape($&,ACCUM) -> [ACCUM,<<"&amp;">>];
escape($<,ACCUM) -> [ACCUM,<<"&lt;">>];
escape($>,ACCUM) -> [ACCUM,<<"&gt;">>];
escape($\r,ACCUM) -> [ACCUM,$\r];
escape($\n,ACCUM) -> [ACCUM,$\n];
escape($\t,ACCUM) -> [ACCUM,$\t];
escape(C,ACCUM) when is_integer(C) ->
  if 16#1F<C, C=<16#7F -> [ACCUM,C];
     true -> [ACCUM,[$&,$#,integer_to_list(C),$;]]
  end.
escape(<<>>,_,INPUT,ACCUM) -> [ACCUM,INPUT];
escape(<<$"/utf8,CONT/bytes>>,N,INPUT,ACCUM) ->
  escape(CONT,0,CONT,[ACCUM,binary_part(INPUT,0,N),<<"&quot;">>]);
escape(<<$&/utf8,CONT/bytes>>,N,INPUT,ACCUM) ->
  escape(CONT,0,CONT,[ACCUM,binary_part(INPUT,0,N),<<"&amp;">>]);
escape(<<$>/utf8,CONT/bytes>>,N,INPUT,ACCUM) ->
  escape(CONT,0,CONT,[ACCUM,binary_part(INPUT,0,N),<<"&gt;">>]);
escape(<<$</utf8,CONT/bytes>>,N,INPUT,ACCUM) ->
  escape(CONT,0,CONT,[ACCUM,binary_part(INPUT,0,N),<<"&lt;">>]);
escape(<<$\r/utf8,CONT/bytes>>,N,INPUT,ACCUM) ->
  escape(CONT,N+1,INPUT,ACCUM);
escape(<<$\n/utf8,CONT/bytes>>,N,INPUT,ACCUM) ->
  escape(CONT,N+1,INPUT,ACCUM);
escape(<<$\t/utf8,CONT/bytes>>,N,INPUT,ACCUM) ->
  escape(CONT,N+1,INPUT,ACCUM);
escape(<<C/utf8,CONT/bytes>>,N,INPUT,ACCUM) ->
  if 16#1F<C, C=<16#7F -> escape(CONT,N+1,INPUT,ACCUM);
     true -> escape(CONT,0,CONT,[ACCUM,binary_part(INPUT,0,N),[<<"&#">>,integer_to_list(C),$;]])
  end.

xml(_Printer,{raw,A}) -> A;
xml(_Printer,{'CDATA',A}) -> [<<"<![CDATA[">>,A,<<"]]>">>];
xml(_Printer,{comment,A}) -> [<<"<!--">>,A,<<"-->">>];
xml(_Printer,#parse_instruction{value=A}) -> [<<"<%">>,A,<<">">>];
xml(_Printer,#meta_instruction{value=A}) -> [<<"<!">>,A,<<">">>];
xml(Printer,As) when is_list(As) -> [xml(Printer,A) || A <- As];
xml(Printer,A) when is_tuple(A), is_atom(element(1,A)) ->
  Mod = element(1,A),
  case erlang:function_exported(Mod,xml,2) of
    true -> Mod:xml(Printer,A);
    false ->
        {Tag,Attrs,Children} = A,
        xml(Tag,Attrs,Children,Printer)
  end;
xml(_Printer,A) -> escape(A).

xml(A) -> xml(new(),A).

xml(Tag,Attrs,Children,P0) ->
  ?debug(Tag),
  ?debug(Attrs),
  ?debug(Children),
  {NSDecl1,TagStr,P1} = name(Tag,P0),
  {NSDecl2,AttrStrs,P2} = 'xml#attributes'(Attrs,P1),
  case Children of
    none -> [$<,TagStr,NSDecl1,NSDecl2,AttrStrs,$/,$>];
    _ -> [$<,TagStr,NSDecl1,NSDecl2,AttrStrs,$>,xml(P2,Children),$<,$/,TagStr,$>]
  end.

  

'xml#attributes'(Attrs,P) ->
  'xml#attributes'(Attrs,P,[],[]).
'xml#attributes'([],P,AttrStrs,NSDecls) ->
    {lists:reverse(NSDecls),lists:reverse(AttrStrs),P};
'xml#attributes'([{Name,Value}|Attrs],P0,AttrStrs,NSDecls) ->
    {NSDecl,AttrStr,P1} = attribute(Name,Value,P0),
    'xml#attributes'(Attrs,P1,[AttrStr|AttrStrs],[NSDecl|NSDecls]).
  




