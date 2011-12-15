<!-- vim: ft=markdown
-->

Userguide for the SAX style API
===============================
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

Notes
-----

* No tag close events are generated for missing close tags.
* The ENTIRE parser will be included in the generated beam.
* You MUST include parserparser.beam in your build path.
* You have to implement all callbacks
* There is no handling of entities (other than what you implement yourself)
* The parser attempts to turn names into (existing) attoms, so use attoms when you want to match names, but expect binaries and atoms otherwise.
* The parser done NOT recognises '&lt;![INCLUDE[ ... ]]&gt;', '&lt;![IGNORE[ ... ]]&gt;' or '&lt;![%entity;[ ... ]]&gt;'...
* The parser dose NOT deal with '&lt;!ENTITY ... &gt;' and '&lt;!ENTITY % ... &gt;' definitions.
* The parser dose NOT handle '&amp;#...;' and '&amp;#x...;' entities.


