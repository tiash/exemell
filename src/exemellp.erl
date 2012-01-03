-module(exemellp).

-include("exemell.hrl").

-callback xml(exemellp:state(),_) -> iolist().

-export([escape/1,new/0,attribute/3,attribute/4,name/2,name_/2,namespace/2,namespace/3,secondary/2,secondary/3,primary/2,xml/2,xml/1,xml/4]).

-define(xml_nsuri,<<"http://www.w3.org/XML/1998/namespace">>).

-behaviour(exemellp).

-type dict(_K,_V) :: dict().
%% And now the extern code....
-export_type([state/0]).
-opaque state() :: {nsuri(),dict(nsuri(),binary())}.

-spec new() -> state().
new() -> {none,dict:store(?xml_nsuri,<<"xml">>,dict:new())}.

-spec primary(nsuri(),state()) -> {binary(),state()}.
primary(URI,Printer={URI,_}) -> {[],Printer};
primary(URI,{_,Secondary}) when is_binary(URI) -> {[<<" xmlns=\"">>,escape(URI),$"],{URI,Secondary}};
primary(URI,Printer) -> primary(iolist_to_binary(URI),Printer).

-spec secondary(nsuri(),state()) -> {binary(),state()} | {binary(),iolist(),state()}.
-spec secondary(binary(),nsuri(),state()) -> {binary(),iolist(),state()}.
secondary(DPrefix,URI,Printer={Primary,Secondary}) when is_binary(URI) ->
  case dict:find(URI,Secondary) of
    {ok,Prefix} -> {Prefix,[],Printer};
    false ->
      NPrefix = uniqueValue(DPrefix,Secondary),
      {NPrefix,[<<" xmlns:">>,NPrefix,$=,$",escape(URI),$"],{Primary,dict:store(URI,NPrefix,Secondary)}}
  end;
secondary(DPrefix,URI,Printer) -> secondary(DPrefix,iolist_to_binary(URI),Printer).

secondary({Prefix,URI},Printer) -> secondary(Prefix,URI,Printer);
secondary(URI,Printer={_,Secondary}) ->
  secondary("ns"++integer_to_list(dict:size(Secondary)),URI,Printer).

uniqueValue(Val,Dict) when is_tuple(Dict) ->
  uniqueValue(Val,[V||{_,V}<-dict:to_list(Dict)]);
uniqueValue(Val,Dict) when is_binary(Val) ->
  case [V||V<-Dict,V==Val] of
    [] -> Val;
    _ -> uniqueValue(<<Val/bytes,"x">>,Dict)
  end;
uniqueValue(Val,Dict) -> uniqueValue(iolist_to_binary(Val),Dict).

-spec namespace(binary()|none,nsuri(),state()) -> {binary(),iolist(),state()} | {iolist(),state()}.
-spec namespace(nsuri(),state()) -> {binary(),iolist(),state()} | {iolist(),state()}.
namespace(_Prefix,URI,Printer={URI,_}) -> {[],Printer};
namespace(DPrefix,URI,Printer={Primary,Secondary}) ->
  case dict:find(URI,Secondary) of
    {ok,Prefix} -> {Prefix,[],Printer};
    false ->
      case DPrefix of
        none -> {[<<" xmlns=\"">>,URI,$"],{URI,Secondary}};
        _  ->
          NPrefix = uniqueValue(DPrefix,Secondary),
          {NPrefix,[<<" xmlns:">>,NPrefix,$=,$",escape(URI),$"],{Primary,dict:store(URI,NPrefix,Secondary)}}
      end
  end.
namespace({Prefix,URI},Printer) -> namespace(Prefix,URI,Printer);
namespace(URI,Printer) -> namespace(none,URI,Printer).

-spec name({binary(),nsuri()}|nsuri(),binary(),state()) -> {iolist(),iolist(),state()}.
-spec name_({binary(),nsuri()}|nsuri(),binary(),state()) -> {iolist(),iolist(),state()}.
name(NS,Tag,P0) ->
  case namespace(NS,P0) of
    {PRE,IO,P1} -> {IO,[PRE,$:,Tag],P1};
    {IO,P1} -> {IO,Tag,P1}
  end.
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

sanitize({A,B}) -> {sanitize(A),sanitize(B)};
sanitize(A) when is_atom(A) -> atom_to_binary(A,utf8);
sanitize(A) when is_binary(A) -> A;
sanitize(As) when is_list(As) -> [sanitize(A) || A<-As].

attribute(NS,Name,Value,P0) ->
  {Pre,IOName,P1} = name(NS,Name,P0),
  {Pre,[$ ,IOName,$=,$",escape(Value),$"],P1}.
attribute(Name,Value,P0) ->
  {Pre,IOName,P1} = name(Name,P0),
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
escape(<<>>,_,INPUT,ACCUM) -> [INPUT,ACCUM];
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
  {NSDecl1,TagStr,P1} = name(sanitize(Tag),P0),
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
    {NSDecl,AttrStr,P1} = attribute(sanitize(Name),Value,P0),
    'xml#attributes'(Attrs,P1,[AttrStr|AttrStrs],[NSDecl|NSDecls]).
  




