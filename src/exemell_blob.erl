-module(exemell_blob).

-include("exemell.hrl").

-callback xml_blob(input()|none,_State,Parser) -> {block()|skip,Parser}
              when Parser :: exemell:state().
