
-include_lib("extern.hrl").

-define(parse_input(Input),parser:binary(Input)).
-type input() :: binary().

-type bodyFun_(Block,State,Parser) :: fun((input()|none,State,Parser) -> {Block,Parser}).
-type bodyFun(State,Block,Attribute,User) :: bodyFun_(Block,State,exemell:state(Block,Attribute,User)).
-type childFun_(Block,State,Parser) :: fun((Block|value()|close,State,Parser) -> {State|Block,Parser}).
-type childFun(State,Block,Attribute,User) :: childFun_(Block,State,exemell:state(Block,Attribute,User)).
-type block_res_(Block,Parser) :: {skip|Block,Parser}.
-type block_res(Block,Attribute,User) :: block_res_(Block,exemell:state(Block,Attribute,User)).
-type block_blob_(State,Block,Parser) :: {blob,bodyFun_(Block,State,Parser),State,Parser}.
-type block_blob(State,Block,Attribute,User) :: block_blob_(State,Block,exemell:state(Block,Attribute,User)).
-type block_children_(State,Block,Parser) :: {children,childFun_(Block,State,Parser),State,Parser}.
-type block_children(State,Block,Attribute,User) :: block_children_(State,Block,exemell:state(Block,Attribute,User)).
-type blockFun_(Block,Attribute,Parser) ::
      fun((nsuri(),tag(),[Attribute],Parser) -> 
            (block_res_(Block,Parser) | block_blob_(any(),Block,Parser) | block_children_(any(),Block,Parser))).
-type blockFun(Block,Attribute,User) :: blockFun_(Block,Attribute,exemell:state(Block,Attribute,User)).

-type simpleBlockFun_(Block,Attribute,Parser) ::
      fun((nsuri(),tag(),[Attribute],[Block|value()]|none,Parser) -> block_res_(Block,Parser)).
-type simpleBlockFun(Block,Attribute,User) :: simpleBlockFun_(Block,Attribute,exemell:state(Block,Attribute,User)).

-type blockFuns_(Block,Attribute,Parser) :: blockFun_(Block,Attribute,Parser) | simpleBlockFun_(Block,Attribute,Parser).
-type blockFuns(Block,Attribute,User) :: blockFuns_(Block,Attribute,exemell:state(Block,Attribute,User)).

-type attrFun_(Attribute,Parser) :: fun((nsuri(),tag(),value(),Parser) -> {Attribute,Parser}).
-type attrFun(Block,Attribute,User) :: attrFun_(Attribute,exemell:state(Block,Attribute,User)).

-record(namespace, {uri, prefix, block, attribute}).
-type namespace(Block,Attribute,User) ::
  #namespace
  { uri :: nsuri()
  , prefix :: binary()
  , block :: blockFuns(Block,Attribute,User)
  , attribute :: attrFun(Block,Attribute,User)
  }.

-type metaFun_(Parser) :: fun((input(),Parser) -> Parser).
-type metaFun(Block,Attribute,User) :: metaFun_(exemell:state(Block,Attribute,User)).
-type applicationFun_(Parser) :: fun((iolist(),Parser) -> Parser).
-type applicationFun(Block,Attribute,User) :: applicationFun_(exemell:state(Block,Attribute,User)).





