-module(exemell_blob).

-include("exemell.hrl").

-callback xml_blob(input()|none,_State,Parser) -> {block()|skip,Parser}
              when Parser :: exemell:state().

-export([xml_blob/3,xml_block/4]).
-export([xml_blob/4]).


xml_blob(Body,{Tag,Attributes,_},Parser) ->
  {{Tag,Attributes,[{raw,Body}]},Parser}.

xml_block(none,Tag,Attrs,State) -> {blob,?MODULE,{Tag,Attrs,none},State};
xml_block(NS,Tag,Attrs,State) -> {blob,?MODULE,{{NS,Tag},Attrs,none},State}.

  
xml_blob(Body,State,Parser,#exemell_blob{xml_blob=Fun}) -> Fun(Body,State,Parser).

