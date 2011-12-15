-compile([{parse_transform,parsetransform_parser}]).

%% Define lots of nice macros so we can use cryptic horrid names the functions
-ifndef(event_callback).
-define(event_callback,event).
-endif.

-ifndef(entrypoint).
-define(entrypoint,parser).
-endif.

-define(event,'#xmlparser#event').
-define(eventp,'#xmlparser#eventp').
-define(event_cont,'#xmlparser#event_cont').
-define(parser,'#xmlparser#parser').
-define(parser_,'#xmlparser#parser_').
-define(cdata,'#xmlparser#cdata').
-define(open,'#xmlparser#open').
-define(application,'#xmlparser#pi').
-define(meta,'#xmlparser#meta').
-define(section,'#xmlparser#sec').
-define(comment,'#xmlparser#comment').
-define(attrs,'#xmlparser#attrs').
-define(entity,'#xmlparser#entity').
-define(close,'#xmlparser#close').
-define(attrval,'#xmlparser#attrval').
-define(attrval_,'#xmlparser#attrval_').
-define(attrval_quote,'#xmlparser#attrval_quote').
-define(attrval_quote_entity,'#xmlparser#attrval_quote_entity').


-define(event_text,'#xmlparser#event#text').
-define(event_meta,'#xmlparser#event#meta').
-define(event_comment,'#xmlparser#event#comment').
-define(event_open_tag,'#xmlparser#event#tag#open').
-define(event_attribute_name,'#xmlparser#event#attribute#name').
-define(event_attribute_value,'#xmlparser#event#attribute#value').
-define(event_attribute_begin_value,'#xmlparser#event#attribute#value#begin').
-define(event_attribute_end_value,'#xmlparser#event#attribute#value#end').
-define(event_attribute_value_entity,'#xmlparser#event#attribute#value#entity').
-define(event_attribute_value_text,'#xmlparser#event#attribute#value#text').
-define(event_end_block,'#xmlparser#event#block#end').
-define(event_entity,'#xmlparser#event#entity').
-define(event_end_section,'#xmlparser#event#section#end').
-define(event_application,'#xmlparser#event#application').
-define(event_begin_section,'#xmlparser#event#section#begin').
-define(event_close_tag_begin_block,'#xmlparser#event#block#begin').
-define(event_close_tag,'#xmlparser#event#tag#close').

-define(nil,<< >>).

-define(WS,[" ","\t","\r","\n"]).


?entrypoint(Input,State) ->
  ?parser(Input,State).

?parser({State,Input}) -> ?parser(Input,State).
?parser(Input,State) -> parser:input(Input);
?parser(<<Text,"<",Input>>,State) ->
  ?open(?event_text(Text,{State,Input}));
?parser(<<Text,"&",Input>>,State) ->
  ?entity(?event_text(Text,{State,Input}));
?parser(<<Text,"</",Input>>,State) ->
  ?close(?event_text(Text,{State,Input}));
?parser(<<Text,"<!--",Input>>,State) ->
  ?comment(?event_text(Text,{State,Input}));
% ?parser(<<Text,"<![CDATA[",Input>>,State) -> 
%   ?cdata(?event_text(Text,{State,Input}));
?parser(<<Text,"<![",Input>>,State) -> 
  ?section(?event_text(Text,{State,Input}));
?parser(<<Text,"]]>",Input>>,State) -> 
  ?parser(?event_end_section(?event_text(Text,{State,Input})));
?parser(<<Text,"<!",Input>>,State) -> 
  ?meta(?event_text(Text,{State,Input}));
?parser(<<Text,"<?",Input>>,State) -> 
  ?application(?event_text(Text,{State,Input}));
?parser(?nil,State) -> {ok,State};
?parser(Text,State) ->
  ?parser(?event_text(Text,{State,?nil})).

% ?cdata({State,Input}) -> ?cdata(Input,State).
% ?cdata(Input,State) -> parser:input(Input);
% ?cdata(<<Chars,"]]>",Input>>,State) ->
%   ?parser(?event_cdata(Chars,{State,Input})).

?application({State,Input}) -> ?application(Input,State).
?application(Input,State) -> parser:input(Input);
?application(<<Instruction,"?>",Input>>,State) ->
  ?parser(?event_application(Instruction,{State,Input})).

?meta({State,Input}) -> ?meta(Input,State).
?meta(Input,State) -> parser:input(Input);
?meta(<<Instruction,">",Input>>,State) ->
  ?parser(?event_meta(Instruction,{State,Input})).

?comment({State,Input}) -> ?comment(Input,State).
?comment(Input,State) -> parser:input(Input);
?comment(<<Comment,"-->",Input>>,State) ->
  ?parser(?event_comment(Comment,{State,Input})).

?section({State,Input}) -> ?section(Input,State).
?section(Input,State) -> parser:input(Input);
?section(<<Name,"[",Input>>,State) ->
  ?parser(?event_begin_section(Name,{State,Input})).
  
?open({State,Input}) -> ?open(Input,State).
?open(Input,State) -> parser:input(Input);
?open(<<Tag,?WS,Input>>,State) ->
  ?attrs(?event_open_tag(Tag,{State,Input}));
?open(<<Tag,">",Input>>,State) -> 
  ?parser(?event_close_tag_begin_block(?event_open_tag(Tag,{State,Input})));
?open(<<Tag,"/>",Input>>,State) ->
  ?parser(?event_close_tag(?event_open_tag(Tag,{State,Input}))).

?attrs({State,Input}) -> ?attrs(Input,State).
?attrs(Input,State) -> parser:input(Input);
?attrs(<<?nil,?WS,Input>>,State) ->
  ?attrs(Input,State);
?attrs(<<Name,?WS,Input>>,State) ->
  ?attrval(?event_attribute_name(Name,{State,Input}));
?attrs(<<?nil,">",Input>>,State) ->
  ?parser(?event_close_tag_begin_block({State,Input}));
?attrs(<<Name,">",Input>>,State) ->
  ?parser(?event_close_tag_begin_block(?event_attribute_value(none,?event_attribute_name(Name,{State,Input}))));
?attrs(<<?nil,"/>",Input>>,State) ->
  ?parser(?event_close_tag({State,Input}));
?attrs(<<Name,"/>",Input>>,State) ->
  ?parser(?event_close_tag(?event_attribute_value(none,?event_attribute_name(Name,{State,Input}))));
?attrs(<<Name,"=",Input>>,State) ->
  ?attrval_(?event_attribute_name(Name,{State,Input})).

?attrval({State,Input}) -> ?attrval(Input,State).
?attrval(Input,State) -> parser:input(Input);
?attrval(<<?nil,?WS,Input>>,State) ->
  ?attrval(Input,State);
?attrval(<<Name,?WS,Input>>,State) ->
  ?attrval(?event_attribute_name(Name,?event_attribute_value(none,{State,Input})));
?attrval(<<?nil,"=",Input>>,State) ->
  ?attrval_(Input,State);
?attrval(<<Name,"=",Input>>,State) ->
  ?attrval_(?event_attribute_name(Name,?event_attribute_value(none,{State,Input})));
?attrval(<<?nil,">",Input>>,State) ->
  ?parser(?event_close_tag_begin_block(?event_attribute_value(none,{State,Input})));
?attrval(<<Name,">",Input>>,State) ->
  ?parser(?event_close_tag_begin_block(?event_attribute_value(none,?event_attribute_name(Name,?event_attribute_value(none,{State,Input})))));
?attrval(<<?nil,"/>",Input>>,State) ->
  ?parser(?event_close_tag(?event_attribute_value(none,{State,Input})));
?attrval(<<Name,"/>",Input>>,State) ->
  ?parser(?event_close_tag(?event_attribute_value(none,?event_attribute_name(Name,?event_attribute_value(none,{State,Input}))))).

?attrval_({State,Input}) -> ?attrval_(Input,State).
?attrval_(Input,State) -> parser:input(Input);
?attrval_(<<?nil,?WS,Input>>,State) -> 
  ?attrval_(Input,State);
?attrval_(<<Val,?WS,Input>>,State) ->
  ?attrs(?event_attribute_value(Val,{State,Input}));
?attrval_(<<?nil,"\"",Input>>,State) ->
  ?attrval_quote(?event_attribute_begin_value({State,Input}));
?attrval_(<<Val,">",Input>>,State) ->
  ?parser(?event_close_tag_begin_block(?event_attribute_value(Val,{State,Input})));
?attrval_(<<Val,"/>",Input>>,State) ->
  ?parser(?event_close_tag(?event_attribute_value(Val,{State,Input}))).

?attrval_quote({State,Input}) -> ?attrval_quote(Input,State).
?attrval_quote(Input,State) -> parser:input(Input);
?attrval_quote(<<Text,"\"",Input>>,State) ->
  ?attrs(?event_attribute_end_value(?event_attribute_value_text(Text,{State,Input})));
?attrval_quote(<<Text,"&",Input>>,State) ->
  ?attrval_quote_entity(?event_attribute_value_text(Text,{State,Input})).

?attrval_quote_entity({State,Input}) -> ?attrval_quote_entity(Input,State).
?attrval_quote_entity(Input,State) -> parser:input(Input);
?attrval_quote_entity(<<Name,";",Input>>,State) ->
  ?attrval_quote(?event_attribute_value_entity(Name,{State,Input})).

?close({State,Input}) -> ?close(Input,State).
?close(Input,State) -> parser:input(Input);
?close(<<Tag,">",Input>>,State) ->
  ?parser(?event_end_block(Tag,{State,Input})).

?entity({State,Input}) -> ?entity(Input,State).
?entity(Input,State) -> parser:input(Input);
?entity(<<Name,";",Input>>,State) ->
  ?parser(?event_entity(Name,{State,Input})).











