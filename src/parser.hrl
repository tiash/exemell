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

-compile([{parse_transform,parser_pt}]).

%% Define lots of nice macros so we can use cryptic horrid names the functions

-ifndef(entrypoint).
-define(entrypoint,parser).
-endif.

-define(parse,'#xmlparser#parser').
-define(parse_,'#xmlparser#parser_').
-define(parse_cdata,'#xmlparser#cdata').
-define(parse_open,'#xmlparser#open').
-define(parse_application,'#xmlparser#pi').
-define(parse_meta,'#xmlparser#meta').
-define(parse_section,'#xmlparser#sec').
-define(parse_comment,'#xmlparser#comment').
-define(parse_attrs,'#xmlparser#attrs').
-define(parse_entity,'#xmlparser#entity').
-define(parse_close,'#xmlparser#close').
-define(parse_attrval,'#xmlparser#attrval').
-define(parse_attrval_,'#xmlparser#attrval_').
-define(parse_attrval_quote,'#xmlparser#attrval_quote').
-define(parse_attrval_quote_entity,'#xmlparser#attrval_quote_entity').


-define(nil,<< >>).

-define(WS,[" ","\t","\r","\n"]).


?entrypoint(Input,State) ->
  ?parse(Input,State).

?parse({State,Input}) -> ?parse(Input,State).
?parse(Input,State) -> ?parse_input(Input);
?parse(<<Text,"<",Input>>,State) ->
  ?parse_open(event_text(Text,{State,Input}));
?parse(<<Text,"&",Input>>,State) ->
  ?parse_entity(event_text(Text,{State,Input}));
?parse(<<Text,"</",Input>>,State) ->
  ?parse_close(event_text(Text,{State,Input}));
?parse(<<Text,"<!--",Input>>,State) ->
  ?parse_comment(event_text(Text,{State,Input}));
% ?parse(<<Text,"<![CDATA[",Input>>,State) -> 
%   ?parse_cdata(event_text(Text,{State,Input}));
?parse(<<Text,"<![",Input>>,State) -> 
  ?parse_section(event_text(Text,{State,Input}));
?parse(<<Text,"]]>",Input>>,State) -> 
  ?parse(event_end_section(event_text(Text,{State,Input})));
?parse(<<Text,"<!",Input>>,State) -> 
  ?parse_meta(event_text(Text,{State,Input}));
?parse(<<Text,"<?parse_",Input>>,State) -> 
  ?parse_application(event_text(Text,{State,Input}));
?parse(<< >>,State) -> {ok,State};
?parse(Text,State) ->
  ?parse(event_text(Text,{State, << >> })).

% ?parse_cdata({State,Input}) -> ?parse_cdata(Input,State).
% ?parse_cdata(Input,State) -> ?parse_input(Input);
% ?parse_cdata(<<Chars,"]]>",Input>>,State) ->
%   ?parse(event_cdata(Chars,{State,Input})).

?parse_application({State,Input}) -> ?parse_application(Input,State).
?parse_application(Input,State) -> ?parse_input(Input);
?parse_application(<<Instruction,"?parse_>",Input>>,State) ->
  ?parse(event_application(Instruction,{State,Input})).

?parse_meta({State,Input}) -> ?parse_meta(Input,State).
?parse_meta(Input,State) -> ?parse_input(Input);
?parse_meta(<<Instruction,">",Input>>,State) ->
  ?parse(event_meta(Instruction,{State,Input})).

?parse_comment({State,Input}) -> ?parse_comment(Input,State).
?parse_comment(Input,State) -> ?parse_input(Input);
?parse_comment(<<Comment,"-->",Input>>,State) ->
  ?parse(event_comment(Comment,{State,Input})).

?parse_section({State,Input}) -> ?parse_section(Input,State).
?parse_section(Input,State) -> ?parse_input(Input);
?parse_section(<<Name,"[",Input>>,State) ->
  ?parse(event_begin_section(Name,{State,Input})).
  
?parse_open({State,Input}) -> ?parse_open(Input,State).
?parse_open(Input,State) -> ?parse_input(Input);
?parse_open(<<Tag,?WS,Input>>,State) ->
  ?parse_attrs(event_open_tag(Tag,{State,Input}));
?parse_open(<<Tag,">",Input>>,State) -> 
  ?parse(event_close_tag_begin_block(event_open_tag(Tag,{State,Input})));
?parse_open(<<Tag,"/>",Input>>,State) ->
  ?parse(event_close_tag(event_open_tag(Tag,{State,Input}))).

?parse_attrs({State,Input}) -> ?parse_attrs(Input,State).
?parse_attrs(Input,State) -> ?parse_input(Input);
?parse_attrs(<< << >>,?WS,Input>>,State) ->
  ?parse_attrs(Input,State);
?parse_attrs(<<Name,?WS,Input>>,State) ->
  ?parse_attrval(event_attribute_name(Name,{State,Input}));
?parse_attrs(<< << >>,">",Input>>,State) ->
  ?parse(event_close_tag_begin_block({State,Input}));
?parse_attrs(<<Name,">",Input>>,State) ->
  ?parse(event_close_tag_begin_block(event_attribute_value(none,event_attribute_name(Name,{State,Input}))));
?parse_attrs(<< << >>,"/>",Input>>,State) ->
  ?parse(event_close_tag({State,Input}));
?parse_attrs(<<Name,"/>",Input>>,State) ->
  ?parse(event_close_tag(event_attribute_value(none,event_attribute_name(Name,{State,Input}))));
?parse_attrs(<<Name,"=",Input>>,State) ->
  ?parse_attrval_(event_attribute_name(Name,{State,Input})).

?parse_attrval({State,Input}) -> ?parse_attrval(Input,State).
?parse_attrval(Input,State) -> ?parse_input(Input);
?parse_attrval(<< << >>,?WS,Input>>,State) ->
  ?parse_attrval(Input,State);
?parse_attrval(<<Name,?WS,Input>>,State) ->
  ?parse_attrval(event_attribute_name(Name,event_attribute_value(none,{State,Input})));
?parse_attrval(<< << >>,"=",Input>>,State) ->
  ?parse_attrval_(Input,State);
?parse_attrval(<<Name,"=",Input>>,State) ->
  ?parse_attrval_(event_attribute_name(Name,event_attribute_value(none,{State,Input})));
?parse_attrval(<< << >>,">",Input>>,State) ->
  ?parse(event_close_tag_begin_block(event_attribute_value(none,{State,Input})));
?parse_attrval(<<Name,">",Input>>,State) ->
  ?parse(event_close_tag_begin_block(event_attribute_value(none,event_attribute_name(Name,event_attribute_value(none,{State,Input})))));
?parse_attrval(<< << >>,"/>",Input>>,State) ->
  ?parse(event_close_tag(event_attribute_value(none,{State,Input})));
?parse_attrval(<<Name,"/>",Input>>,State) ->
  ?parse(event_close_tag(event_attribute_value(none,event_attribute_name(Name,event_attribute_value(none,{State,Input}))))).

?parse_attrval_({State,Input}) -> ?parse_attrval_(Input,State).
?parse_attrval_(Input,State) -> ?parse_input(Input);
?parse_attrval_(<< << >>,?WS,Input>>,State) -> 
  ?parse_attrval_(Input,State);
?parse_attrval_(<<Val,?WS,Input>>,State) ->
  ?parse_attrs(event_attribute_value(Val,{State,Input}));
?parse_attrval_(<< << >>,"\"",Input>>,State) ->
  ?parse_attrval_quote(event_attribute_begin_value({State,Input}));
?parse_attrval_(<<Val,">",Input>>,State) ->
  ?parse(event_close_tag_begin_block(event_attribute_value(Val,{State,Input})));
?parse_attrval_(<<Val,"/>",Input>>,State) ->
  ?parse(event_close_tag(event_attribute_value(Val,{State,Input}))).

?parse_attrval_quote({State,Input}) -> ?parse_attrval_quote(Input,State).
?parse_attrval_quote(Input,State) -> ?parse_input(Input);
?parse_attrval_quote(<<Text,"\"",Input>>,State) ->
  ?parse_attrs(event_attribute_end_value(event_attribute_value_text(Text,{State,Input})));
?parse_attrval_quote(<<Text,"&",Input>>,State) ->
  ?parse_attrval_quote_entity(event_attribute_value_text(Text,{State,Input})).

?parse_attrval_quote_entity({State,Input}) -> ?parse_attrval_quote_entity(Input,State).
?parse_attrval_quote_entity(Input,State) -> ?parse_input(Input);
?parse_attrval_quote_entity(<<Name,";",Input>>,State) ->
  ?parse_attrval_quote(event_attribute_value_entity(Name,{State,Input})).

?parse_close({State,Input}) -> ?parse_close(Input,State).
?parse_close(Input,State) -> ?parse_input(Input);
?parse_close(<<Tag,">",Input>>,State) ->
  ?parse(event_end_block(Tag,{State,Input})).

?parse_entity({State,Input}) -> ?parse_entity(Input,State).
?parse_entity(Input,State) -> ?parse_input(Input);
?parse_entity(<<Name,";",Input>>,State) ->
  ?parse(event_entity(Name,{State,Input})).











