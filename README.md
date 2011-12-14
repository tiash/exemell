
Yet another XML Parser
=====================

This parser was built with two main goals:
* binary parsing
* Namespace specific parsing

It also features a general SAX style XML parser as an includable header (this parser is very primitive though) and a _parse transform_ (aka don't write these) that generates simple but fast binary matchers.

Userguide (Simple SAX style)
---------------------
Best given by a code snippet (that will, more or less echo the original document).

    -module(exSAX)
    -export([test/0]).

    -type state() :: term().

    -include_lib("exemell/include/simple.hrl").
    
    test() ->
      XML = <<
        "<root>\n",
        "  <block a=b/>\n",
        "  <block b="a"/>\n",
        "  <badBlock x> <block> <ns:a xmlns:ns="http://example.com/a"/> <b/> </block>\n"
        "</root>\n">>,
      test(XML).
    
    %% More or less echo's its input
    test(XML) ->
      parse(XML,state).

    -type tag() :: binary()|atom().
    
    %% Called at the start of every element
    -spec ?sax_element_start(tag(),[{tag(),iolist()}],state()) -> state().
    %% Ocasionally an iolist...
    ?sax_element_start(Tag,Attrs,State) ->
      [State,iolib:format("<~s",[Tag]),
      [iolib:format(" ~s=~s",[Name,Value]) || {Name,Value} <- Attrs],
      ,iolib:format(">")].
    
    %% Called at the end of child less elements and for every closing tag, never called for unclosed tags
    -spec ?sax_element_end(tag(),state()) -> state().
    ?sax_element_end(Tag,State) ->
      [State,iolib:format("</~s>",[Tag])].

    %% Called for every sequence of characters, whitespace included between two tags/entities.
    -spec ?sax_text(binary(),state()) -> state().
    ?sax_text(Text,State) ->
      [State,iolib:format("~s",[Text])].
    
    %% Called for every entity
    -spec ?sax_entity(tag(),state()) -> state().
    ?sax_entity(Name,State) ->
      [State,iolib:format("&~s;",[Text])].

    %% Called for processing instructions
    -spec ?sax_processinginstruction(binary(),state()) -> state().
    ?sax_processinginstruction(ProcessingInstruction,State) ->
      [State,iolib:format("<?~s?>",[ProcessingInstruction])].

    %% Called for comments
    -spec ?sax_comment(binary(),state()) -> state().
    ?sax_comment(Comment,State) -> 
      [State,iolib:format("<!--~s-->",[Comment])].

    %% Called for DTD instructions
    -spec ?sax_meta(binary(),state()) -> state().
    ?sax_meta(DTDInstruction,State) ->
      [State,iolib:format("<!~s>",[DTDInstruction])].

    %% Called for CDATA sections
    -spec ?sax_cdata(binary(),state()) -> state().
    ?sax_cdata(CDATA,State) ->
      [State,iolib:format("<![CDATA[~s]]>",[CDATA])].
    
    %% Called for each section start, the XML spec only defined INCLUDE and IGNORE, plus symbolic versions.
    -spec ?sax_open_section(binary(),state()) -> state().
    ?sax_open_section(tag(),State) ->
      [State,iolib:format("<![~s[",[Tag])].
    
    -spec ?sax_close_section(state()) -> state().
    ?sax_close_section(State) ->
      [State,iolib:format("]]>")],

    %% This is really not very sax.... but it seems only sane way to implement escaping in attributes
    -spec ?sax_quote_entity(tag(),state()) -> iolist().
    ?sax_quote_entity(lt,State) -> {"<",State};
    ?sax_quote_entity(gt,State) -> {">",State};
    ?sax_quote_entity(amp,State) -> {"&",State};
    ?sax_quote_entity(quot,State) -> {"\"",State};
    ?sax_quote_entity(Val,State) when is_atom(Val) -> {["&",atom_to_list(Val),";"],State};
    ?sax_quote_entity(Val,State) -> {["&",Val,";"],State}.

The full parser has a similar API, but gets an additional argument (and result) the remaining input; dosen't recognise CDATA sections, discriminates between empty tags, and generates separate events for attributes (quoted attributes get LOTS of events), see 'exemell/include/sax.hrl' for the full details.

### Notes
* No tag close events are generated for missing close tags.
* The ENTIRE parser will be included in the generated beam.
* You MUST include parserparser.beam in your build path.
* You have to implement all callbacks
* There is no handling of entities (other than what you implement yourself)

Userguide (Namespace Aware SAX + DOMish)
-------------------------------------------
If no namespaces are installed then the parser will revert to a DOM like style (see 'exemell/include/xml.hrl' for the format details)
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
      {
    

### Notes
* The parser attempts to turn names into (existing) attoms, so use attoms when you want to match names, but expect binaries and atoms otherwise.
* 

    

    
    
    

### Notes




Performance
-----------

It's not the fastest parser out there, but its reasonably fast a very rough test places the parser at about:
 * ~10% slower then erlsom
 * twice as fast as xmerl
As with any performance rating, take them with a lot of salt, and do your own measurments.

Caviats
-------
* No validation other then basic wellformedness is done (the parser will silently ignore missing close tags)
* The parser is hard coded to UTF8 encoding






