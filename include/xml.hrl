%% The following defines give a chance to overrule the defaults
-ifndef(INCLUDE_XML).
-define(INCLUDE_XML,true).
-ifndef(entity).
-define(entity,value()).
-endif.

-ifndef(val).
-define(val, entity() | binary()).
-endif.

-ifndef(attribute).
-define(attribute,{nstag(),value()}).
-endif.

-ifndef(block).
-define(block,{ nstag(), attributes(), children() }).
-endif.

-ifndef(child).
-define(child,block() | value()).
-endif.

-type nsuri() :: binary() | none.
-type tag() :: atom() | binary().
-type nstag() :: tag() | {nsuri(),tag()}.
-type attribute() :: ?attribute.
-type attributes() :: [attribute()].
-type entity() :: ?entity.
-type block() :: ?block.
-type child() :: ?child.
-type children() :: maybe_improper_list(children(),children()) | child().
-type val() :: ?val.
-type value() :: maybe_improper_list(value(),value()) | val().
-endif.






