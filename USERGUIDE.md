<!-- vim: ft=markdown
-->

Userguide (Namespace Aware SAX + DOMish)
-------------------------------------------
If no namespaces are installed then the parser will revert to a DOM like style (see `exemell/include/xml.hrl` for the format details)
Again, commented code is probably simplest.

    -module(exNS).

    -export([test/0]).

    -include("exemell/include/ns.hrl").

    test() ->
      Config1 = exemell:newState(),
      Config2 = exemell:installNamespace(null_namespace(),Config1),
      Config3 = exemell:installNamespace(my_namespace(),Config2),
      %% undefined namespaces will be generated automatically to produce DOM style results
      Config4 = exemell:installEntity(<<"nbsp">>,<<" ">>,Config3),
      Config5 = exemell:installEntity(<<"lt">>,<<"<">>,Config4),
      Config6 = exemell:installEntity(<<"gt">>,<<"<">>,Config5),
      Config7 = exemell:installEntity(<<"amp">>,<<"<">>,Config6),
      Config8 = exemell:installEntity(<<"quot">>,<<"<">>,Config7),
      Config9 = exemell:userstate(0,Config8),
      %% You can share the Config with other processes and recycle it,
      %% The config is used for internalising binaries() and will grow to include any other generated namespaces and anything you do with the global state.
      {ok,Result,Config10} = exemell:parse(FILE,Config9),
      io:format(

    null_namespace() ->
      exemell:newNamespace(none,fun null_namespace_block/4).
      %% same as #namespace{ uir=none, block=fun null_namespace_block/4, attribute=fun exemell:attribute/4 },

    %% Uri will ALWAYS correspond to the namespace.
    null_namespace_block(none,Tag,Attribtues,Children,State) ->
      {{Tag,Attributes,Children},State}. % dose (mostly) the same as exemell:block/4, which is equivalant to exemell:block/3

    my_namespace() ->
      #namespace{ uri=<<"http://example.com/namespace">>
                , block=fun my_namespace_block/3
                , attribute=fun my_namespace_attribute/3 % could also
                }.
    
    %% Uri will be the namespace for prefixed attributes, and non for non prefixed attributes.
    %% The results of this get sent to the block function
    my_namespace_attribute(_URI,number,ID,State) ->
      {list_to_integer(binary_to_list(iolist_to_binary(ID))),State};
    my_namespace_attribute(_URI,skipme,_VAL,State) ->
      {skip,State};
    my_namespace_attribute(_URI,pink,VAL,State) ->
      {{violet,Val},State};
    my_namespace_attribute(none,incr,_,State) ->
      {skip,State+1};
    my_namespace_attribute(URI,decr,_,State) ->
      {skip,State-1};
    my_namespace_attribute(URI,ns,_,State) ->
      {{ns,URI},State};
    my_namespace_attribute(_URI,Name,Value,State) ->
      {{Name,Value},State}.




### Notes
* The parser attempts to turn names into (existing) attoms, so use attoms when you want to match names, but expect binaries and atoms otherwise.
* The parser recognises `<![INCLUDE[ ... ]]>` and `<![IGNORE[ ... ]]>` and `<![%entity;[ ... ]]>` sections (if the appropriate entities have been defined).
* The parser recognises `<!ENTITY ... >` and `<!ENTITY % ... >` definitions (the handling can be overwritten).
* The parser dose not recognise `<!DOCTYPE ... >` or any other DTD definitions.
* The parser recognises `&#...;` and `&#x...;` entities, but no other entities are recognised (unless installed manually or by a `<!ENTITY ... >`).


