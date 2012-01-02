-module(exemellp).

-include("exemell.hrl").

-callback xml(_,exemell:state()) -> iolist().

-export([escape/1,new/0,attribute/3,attribute/4,name/2,namespace/2,namespace/3,secondary/2,secondary/3,primary/2,xml/2]).

-define(xml_nsuri,<<"http://www.w3.org/XML/1998/namespace">>).

-behaviour(exemellp).

-type dict(_K,_V) :: dict().
%% And now the extern code....
-record(?MODULE,{primary::nsuri(),secondary::dict(nsuri(),binary()),parent::state()|undefined}).
-define(state,#?MODULE).
-export_type([state/0]).
-opaque state() :: ?state{}.

-spec new() -> state().
new() -> 
  ?state{primary=none,secondary=dict:store(?xml_nsuri,<<"xml">>,dict:new())}.

-spec primary(nsuri(),state()) -> {binary(),state()}.
primary(URI,PRINTER=?state{primary=URI}) -> {[],PRINTER};
primary(URI,PRINTER) when is_binary(URI) -> {[<<" xmlns=\"">>,escape(URI),$"],PRINTER?state{primary=URI}};
primary(URI,PRINTER) -> primary(iolist_to_binary(URI),PRINTER).

-spec secondary(nsuri(),state()) -> {binary(),state()}.
-spec secondary(binary(),nsuri(),state()) -> {binary(),iolist(),state()}.
secondary(DPREFIX,URI,PRINTER=?state{secondary=SECONDARY}) when is_binary(URI) ->
  case dict:find(URI,SECONDARY) of
    {ok,PREFIX} -> {PREFIX,[],PRINTER};
    false ->
      NPREFIX = uniqueValue(DPREFIX,SECONDARY),
      {NPREFIX,[<<" xmlns:">>,NPREFIX,$=,$",escape(URI),$"],PRINTER?state{secondary=dict:store(URI,NPREFIX,SECONDARY)}}
  end;
secondary(DPREFIX,URI,PRINTER) -> secondary(DPREFIX,iolist_to_binary(URI),PRINTER).

secondary({PREFIX,URI},PRINTER) -> secondary(PREFIX,URI,PRINTER);
secondary(URI,PRINTER=?state{secondary=SECONDARY}) ->
  secondary("ns"++integer_to_list(dict:size(SECONDARY)),URI,PRINTER).
uniqueValue(Val,Dict) when is_binary(Val) ->
  case [ x || {_,V} <- dict:to_list(Dict), V==Val] of
    [] -> Val;
    _ -> uniqueValue(<<Val/bytes,"x">>,Dict)
  end;
uniqueValue(Val,Dict) -> uniqueValue(iolist_to_binary(Val),Dict).

-spec namespace(binary()|none,nsuri(),state()) -> {binary(),iolist(),state()} | {iolist(),state()}.
-spec namespace(nsuri(),state()) -> {binary(),iolist(),state()} | {iolist(),state()}.
namespace(_PREFIX,URI,PRINTER=?state{primary=URI}) -> {[],PRINTER};
namespace(DPREFIX,URI,PRINTER=?state{secondary=SECONDARY}) ->
  case dict:find(URI,SECONDARY) of
    {ok,PREFIX} -> {PREFIX,[],PRINTER};
    false ->
      case DPREFIX of
        none -> {[<<" xmlns=\"">>,URI,$"],PRINTER?state{primary=URI}};
        _  ->
          NPREFIX = uniqueValue(DPREFIX,SECONDARY),
          {NPREFIX,[<<" xmlns:">>,NPREFIX,$=,$",escape(URI),$"],PRINTER?state{secondary=dict:store(URI,NPREFIX,SECONDARY)}}
      end
  end.
namespace({PREFIX,URI},PRINTER) -> namespace(PREFIX,URI,PRINTER);
namespace(URI,PRINTER) -> namespace(none,URI,PRINTER).

-spec name({binary(),nsuri()}|nsuri(),binary(),state()) -> {iolist(),iolist(),state()}.
name(NS,TAG,P0) ->
  case namespace(NS,P0) of
    {PRE,IO,P1} -> {IO,[PRE,$:,TAG],P1};
    {IO,P1} -> {IO,TAG,P1}
  end.
-spec name(binary()|{nsuri(),binary()}|{{binary(),nsuri()},binary()},state()) -> {iolist(),iolist(),state()}.
name({NS,TAG},P0) -> name(NS,TAG,P0);
name(TAG,PRINTER) -> name(none,TAG,PRINTER).

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

xml({raw,A},_Printer) -> A;
xml(As,Printer) when is_list(As) -> [xml(A,Printer) || A <- As];
xml(A,Printer) when is_tuple(A), is_atom(element(1,A)) ->
  Mod = element(1,A),
  case erlang:function_exported(Mod,xml,2) of
    true -> Mod:xml(A,Printer);
    false ->
      case A of
        {Tag,Attrs,Children} ->
          {NSDecl1,TagStr,P1} = name(sanitize(Tag),Printer),
          {NSDecl2,AttrStrs,P2} = 'xml#attributes'(Attrs,P1),
          case Children of
            none -> [$<,TagStr,NSDecl1,NSDecl2,AttrStrs,$/,$>];
            _ -> [$<,TagStr,NSDecl1,NSDecl2,AttrStrs,$>,xml(Children,P2),$<,$/,TagStr,$>]
          end
      end
  end;
xml(A,_Printer) -> escape(A).

'xml#attributes'(Attrs,P) ->
  'xml#attributes'(Attrs,P,[],[]).
'xml#attributes'([],P,AttrStrs,NSDecls) ->
    {lists:reverse(NSDecls),lists:reverse(AttrStrs),P};
'xml#attributes'([{Name,Value}|Attrs],P0,AttrStrs,NSDecls) ->
    {NSDecl,AttrStr,P1} = attribute(sanitize(Name),Value,P0),
    'xml#attributes'(Attrs,P1,[AttrStr|AttrStrs],[NSDecl|NSDecls]).
            
  

