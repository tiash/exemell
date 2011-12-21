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






