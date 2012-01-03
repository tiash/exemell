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

-module(exemell_block).

-include("exemell.hrl").

-callback xml_end(_State,Parser) -> {block()|skip,exemell:state()}
              when Parser :: exemell:state().
-callback xml_child(child(),State,Parser) -> {State,exemell:state()}
              when Parser :: exemell:state().

% -callback xml_block(block(),exemellp:state()) -> iolist().

-export([xml_end/2,xml_child/3,xml_block/4]).
-export([xml_end/3,xml_child/4]).

-behaviour(exemell_block).

-spec xml_end(_State,Parser) -> {block()|skip,exemell:state()}
              when Parser :: exemell:state().
-spec xml_child(child(),State,Parser) -> {State,exemell:state()}
              when Parser :: exemell:state().

xml_end({Tag,Attributes,Children},Parser) ->
  {{Tag,Attributes,lists:reverse(Children)},Parser}.
xml_child(Child,{Tag,Attributes,Children},Parser) ->
  {{Tag,Attributes,[Child|Children]},Parser}.

% -spec xml_attribute(attribute(),exemell:state()) -> {iolist(),exemell:state()}.
% xml_attribute({NAME,VALUE},PRINTER) -> exemell:attribute(sane_name(NAME),VALUE,PRINTER).


xml_block(none,Tag,Attrs,State) -> {children,?MODULE,{Tag,Attrs,[]},State};
xml_block(NS,Tag,Attrs,State) -> {children,?MODULE,{{NS,Tag},Attrs,[]},State}.
% -spec xml_block(child(),exemell:state()) -> iolist().
% xml_block({_TAG,ATTRS,BODY},P0) ->
  % {TAG,IO0,P1} = exemell:tag(sane_name(_TAG),P0),
  % {IO1,P2} = lists:foldl(fun (A,{IO,PN}) -> {AIO,PM} = xml_attribute(A,PN), {[IO|AIO],PM} end, {[$<,TAG|IO0],P1}, ATTRS),
  % case BODY of
    % none -> [IO1,$/,$>];
    % {raw,RAW} -> [IO1,$>,RAW,$<,$/,TAG,$>];
    % _ -> [IO1,[xml_block(C,P2) || C<-BODY],$<,$/,TAG,$>]
  % end;
% xml_block({raw,RAW},_P) -> RAW;
% xml_block(TEXT,_P) -> exemell:escape(TEXT).
  
% sane_name(TAG) when is_atom(TAG) -> atom_to_binary(TAG,utf8);
% sane_name(TAG) when is_binary(TAG) -> TAG;
% sane_name({A,B}) -> {sane_name(A),sane_name(B)}.

xml_child(Child,State,Parser,#exemell_block{xml_child=Fun}) -> Fun(Child,State,Parser).
xml_end(State,Parser,#exemell_block{xml_end=Fun}) -> Fun(State,Parser).

