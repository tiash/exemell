-module(exemell_parser).

-include("exemell.hrl").

-callback xml_application(input(),Parser) -> Parser when Parser :: exemell:state().
-callback xml_meta(input(),Parser) -> Parser when Parser :: exemell:state().

-export([xml_application/2,xml_meta/2]).
-behaviour(exemell_parser).

-spec xml_application(input(),Parser) -> Parser when Parser :: exemell:state().
xml_application(Application,Parser) ->
  exemell:addChild(#parse_instruction{value=Application},Parser).
-spec xml_meta(input(),Parser) -> Parser when Parser :: exemell:state().
xml_meta(Meta,Parser) ->
  exemell:addChild(#meta_instruction{value=Meta},Parser).



