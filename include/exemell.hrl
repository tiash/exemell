% vim: cc=80 ft=erlang ts=2 sw=2 et
%
% Copyright 1992-2011 Matthias Horn. All rights reserved.
% 
% Redistribution and use in source and binary forms, with or without
% modification, are permitted provided that the following conditions are met:
% 
% 1. Redistributions of source code must retain the above copyright notice,
%    this list of conditions and the following disclaimer.
% 2. Redistributions in binary form must reproduce the above copyright notice,
%    this list of conditions and the following disclaimer in the documentation
%    and/or other materials provided with the distribution.
% 
% THIS SOFTWARE IS PROVIDED BY THE FREEBSD PROJECT "AS IS" AND ANY EXPRESS OR
% IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
% MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO
% EVENT SHALL THE FREEBSD PROJECT OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
% INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
% (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
% ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
% 
% The views and conclusions contained in the software and documentation are
% those of the authors and should not be interpreted as representing official
% policies, either expressed or implied, of Matthias Horn.
% 

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





