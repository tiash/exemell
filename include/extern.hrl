%% Giving alternative definitions for entity/attribute may be helpful under certain circumstances

-ifndef(child).
-define(child,block() | value() | #parse_instruction{} | #meta_instruction{}).
-endif.

-record(parse_instruction,{value::binary()}).
-record(meta_instruction,{value::binary()}).

-include("xml.hrl").






