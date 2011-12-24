-module(exemell_block).

-include("exemell.hrl").


-callback xml_child(child(),State,Parser) -> {State,exemell:state()}
              when Parser :: exemell:state()
            ;  (close,_State,Parser) -> {block()|skip,exemell:state()}
              when Parser :: exemell:state().
