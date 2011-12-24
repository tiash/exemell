-module(exemell_namespace).

-include("exemell.hrl").

-callback xml_block(nsuri(),tag(),[attribute()],Parser) ->
  {blob, module(), term(), Parser}
| {children, module(), term(), Parser}
  when Parser :: exemell:state().
-callback xml_attribute(nsuri(),tag(),value(),Parser) ->
  {skip,Parser} | {attribute(),Parser}
  when Parser :: exemell:state().



