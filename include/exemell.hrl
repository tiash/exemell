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


-define(parse_input(Input),parser:binary(Input)).
-type input() :: binary().


%% How to tell dialyzer that these should implement the appropriate behaviours.

-record(parse_instruction,{value::binary()}).
-record(meta_instruction,{value::binary()}).

-ifndef(child).
-define(child,block() | value() | #parse_instruction{} | #meta_instruction{}).
-endif.

-include("xml.hrl").

-type callback(_Behaviour) :: module().

%% Rediculus abuse of parametric modules...
%% Piggy-back on the parametrized module mechanism
-record(exemell_blob,
    { xml_blob :: fun((input()|none , any(), exemell:state())
                   -> {block()|skip, exemell:state()})
    }).
-type exemell_blob() :: #exemell_blob{} | callback(exemell_blob).
-record(exemell_block,
    { xml_child :: fun((child(), any(), exemell:state())
                     -> {any(), exemell:state()})
    , xml_end :: fun((any(),exemell:state())
                   -> {block()|skip, exemell:state()})
    }).
-type exemell_block() :: #exemell_block{} | callback(exemell_block).
-record(exemell_namespace,
    { xmlns :: nsuri()
    , xml_prefix :: binary() | none
    , xml_block :: fun((nsuri(),tag(),[attribute()],exemell:state())
                     -> {blob, exemell_blob(), term(), exemell:state()}
                      | {children, exemell_block(), term(), exemell:state()}
                    )
    , xml_attribute :: fun((nsuri(),tag(),value(),exemell:state())
                         -> {skip,exemell:state()} | {attribute(),exemell:state()}
                        )
    }).
-type namespace() :: #exemell_namespace{} | callback(exemell_namespace).
-record(exemell_parser,
    { xml_application :: fun((input(),exemell:state()) -> exemell:state())
    , xml_meta :: fun((input(),exemell:state()) -> exemell:state())
    }).
-type exemell_parser() :: #exemell_parser{} | callback(exemell_parser).







