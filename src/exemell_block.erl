-module(exemell_block).

-include("exemell.hrl").

-callback xml_end(_State,Parser) -> {block()|skip,exemell:state()}
              when Parser :: exemell:state().
-callback xml_child(child(),State,Parser) -> {State,exemell:state()}
              when Parser :: exemell:state().

-export([xml_end/2,xml_child/3]).

-behaviour(exemell_block).

-spec xml_end(_State,Parser) -> {block()|skip,exemell:state()}
              when Parser :: exemell:state().
-spec xml_child(child(),State,Parser) -> {State,exemell:state()}
              when Parser :: exemell:state().


xml_end({Tag,Attributes,Children},Parser) ->
  {{Tag,Attributes,lists:reverse(Children)},Parser}.
xml_child(Child,{Tag,Attributes,Children},Parser) ->
  {{Tag,Attributes,[Child|Children]},Parser}.


