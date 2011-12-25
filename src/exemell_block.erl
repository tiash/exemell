-module(exemell_block).

-include("exemell.hrl").

-callback xml_end(_State,Parser) -> {block()|skip,exemell:state()}
              when Parser :: exemell:state().
-callback xml_child(child(),State,Parser) -> {State,exemell:state()}
              when Parser :: exemell:state().

-callback xml_block(block(),exemellp:state()) -> iolist().

-export([xml_end/2,xml_child/3,xml_block/2]).

-behaviour(exemell_block).

-spec xml_end(_State,Parser) -> {block()|skip,exemell:state()}
              when Parser :: exemell:state().
-spec xml_child(child(),State,Parser) -> {State,exemell:state()}
              when Parser :: exemell:state().

xml_end({Tag,Attributes,Children},Parser) ->
  {{Tag,Attributes,lists:reverse(Children)},Parser}.
xml_child(Child,{Tag,Attributes,Children},Parser) ->
  {{Tag,Attributes,[Child|Children]},Parser}.

-spec xml_attribute(attribute(),exemell:state()) -> {iolist(),exemell:state()}.
xml_attribute({NAME,VALUE},PRINTER) -> exemell:attribute(sane_name(NAME),VALUE,PRINTER).


-spec xml_block(child(),exemell:state()) -> iolist().
xml_block({_TAG,ATTRS,BODY},P0) ->
  {TAG,IO0,P1} = exemell:tag(sane_name(_TAG),P0),
  {IO1,P2} = lists:foldl(fun (A,{IO,PN}) -> {AIO,PM} = xml_attribute(A,PN), {[IO|AIO],PM} end, {[$<,TAG|IO0],P1}, ATTRS),
  case BODY of
    none -> [IO1,$/,$>];
    {raw,RAW} -> [IO1,$>,RAW,$<,$/,TAG,$>];
    _ -> [IO1,[xml_block(C,P2) || C<-BODY],$<,$/,TAG,$>]
  end;
xml_block({raw,RAW},_P) -> RAW;
xml_block(TEXT,_P) -> exemell:escape(TEXT).
  
sane_name(TAG) when is_atom(TAG) -> atom_to_binary(TAG,utf8);
sane_name(TAG) when is_binary(TAG) -> TAG;
sane_name({A,B}) -> {sane_name(A),sane_name(B)}.

