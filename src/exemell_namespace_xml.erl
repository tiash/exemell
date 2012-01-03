-module(exemell_namespace_xml).

-behaviour(exemell_namespace).

-export([xmlns/0,xml_prefix/0,xml_attribute/4,xml_block/4]).
xmlns() -> <<"http://www.w3.org/XML/1998/namespace">>.
xml_prefix() -> <<"xml">>.
xml_attribute(_NS,Name,Value,State) -> {{{?MODULE,Name},Value},State}.
xml_block(_NS,_Tag,_Attrs,_State) -> error.
