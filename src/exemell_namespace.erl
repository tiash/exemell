-module(exemell_namespace).

-include("exemell.hrl").

-callback xml_block(nsuri(),tag(),[attribute()],Parser) ->
  {blob, module(), term(), Parser}
| {children, module(), term(), Parser}
  when Parser :: exemell:state().
-callback xml_attribute(nsuri(),tag(),value(),Parser) ->
  {skip,Parser} | {attribute(),Parser}
  when Parser :: exemell:state().
-callback xmlns() -> nsuri().
-callback xml_prefix() -> binary() | none.

-behaviour(exemell_namespace).

-export([xmlns/0,xml_prefix/0,xml_attribute/4,xml_block/4]).
-export([xmlns/1,xml_prefix/1,xml_attribute/5,xml_block/5]).
-export([new/0,new/1,new/2,new/4,is/1]).

-spec xml_block(nsuri(),tag(),[attribute()],Parser) ->
  {blob, module(), term(), Parser}
| {children, module(), term(), Parser}
  when Parser :: exemell:state().
-spec xml_attribute(nsuri(),tag(),value(),Parser) ->
  {skip,Parser} | {attribute(),Parser}
  when Parser :: exemell:state().

new() -> new(none,none).
new(URI) -> new(none,URI).
new(none,none) -> exemell_namespace;
new(Prefix,URI) -> new(Prefix,URI,fun xml_attribute/4,fun xml_block/4).
new(Prefix,URI,Attrs,Blocks) ->
  #exemell_namespace{xmlns=URI,xml_prefix=Prefix,xml_block=Blocks,xml_attribute=Attrs}.

xmlns() -> none.
xml_prefix() -> none.

xml_block(none,Tag,Attributes,Parser) ->
  {children,exemell_block,{Tag,Attributes,[]},Parser};
xml_block(Uri,Tag,Attributes,Parser) ->
  {children,exemell_block,{{Uri,Tag},Attributes,[]},Parser}.


xml_attribute(none,Name,Value,Parser) -> {{Name,Value},Parser};
xml_attribute(NS,Name,Value,Parser) -> {{{NS,Name},Value},Parser}.

xmlns(#exemell_namespace{xmlns=NS}) -> NS.
xml_prefix(#exemell_namespace{xml_prefix=Prefix}) -> Prefix.
xml_block(NS,Tag,Attrs,Parser,#exemell_namespace{xml_block=Fun}) ->
  Fun(NS,Tag,Attrs,Parser).
xml_attribute(NS,Name,Value,Parser,#exemell_namespace{xml_attribute=Fun}) ->
  Fun(NS,Name,Value,Parser).


is(#exemell_namespace{}) -> true;
is(Mod) when is_atom(Mod) ->
  erlang:function_exported(Mod,xmlns,4)
  andalso
  erlang:function_exported(Mod,xml_prefix,4)
  andalso
  erlang:function_exported(Mod,xml_attribute,4)
  andalso
  erlang:function_exported(Mod,xml_block,4);
is(_) -> false.


