-module(exemell_namespace).

-include("exemell.hrl").

-callback xml_block(nsuri(),tag(),[attribute()],Parser) ->
  {blob, module(), term(), Parser}
| {children, module(), term(), Parser}
  when Parser :: exemell:state().
-callback xml_attribute(nsuri(),tag(),value(),Parser) ->
  {skip,Parser} | {attribute(),Parser}
  when Parser :: exemell:state().

-behaviour(exemell_namespace).

-export([xml_attribute/4,xml_block/4]).
-export([xml_attribute/5,xml_block/5]).

-spec xml_block(nsuri(),tag(),[attribute()],Parser) ->
  {blob, module(), term(), Parser}
| {children, module(), term(), Parser}
  when Parser :: exemell:state().
-spec xml_attribute(nsuri(),tag(),value(),Parser) ->
  {skip,Parser} | {attribute(),Parser}
  when Parser :: exemell:state().

xml_block(none,Tag,Attributes,Parser) ->
  {children,exemell_block,{Tag,Attributes,[]},Parser};
xml_block(Uri,Tag,Attributes,Parser) ->
  {children,exemell_block,{{Uri,Tag},Attributes,[]},Parser}.


xml_attribute(none,Name,Value,Parser) -> {{Name,Value},Parser};
xml_attribute(NS,Name,Value,Parser) -> {{{NS,Name},Value},Parser}.

xml_block(NS,Tag,Attrs,Parser,#exemell_namespace{xml_block=Fun}) ->
  Fun(NS,Tag,Attrs,Parser).
xml_attribute(NS,Name,Value,Parser,#exemell_namespace{xml_attribute=Fun}) ->
  Fun(NS,Name,Value,Parser).


