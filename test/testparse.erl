-module(testparse).

-export([run/0]).
-export([run_/0]).
run() ->
  c:c(?MODULE),
  ?MODULE:run_().
run_() ->
  c:c("../plugins/parsetransform_parser.erl"),
  c:c(parse_test,[{i,"../include"}]),
  {ok,XML1} = file:read_file("/Users/tias/Documents/moonlight/BL/src/com/lionet/projects/fas/data/test/editor/server/testbankfs/eLearning/OriginalCourse.xml"),
  {ok,XML2} = file:read_file("/Users/tias/Desktop/Scratch/QTITest2/qti19.xml"),
  {ok,XML3} = file:read_file("/Users/tias/Documents/moonlight/BL/src/com/lionet/database/data/services/createTables.xml"),
  XMLs = [XML1,XML2,XML3],
  TXTs = [binary_to_list(XML) || XML<-XMLs],
  % {
  % }
  io:format("Startin tests..."),
  { [ begin
  timer:tc(ntimes(5,fun () -> parse_test:xml(XML), ok end)),
  timer:tc(ntimes(10,fun () -> parse_test:xml(XML), ok end))
  end || XML<-XMLs ] , [ begin
  timer:tc(ntimes(5,fun () -> xmerl_scan:string(TXT), ok end)),
  timer:tc(ntimes(10,fun () -> xmerl_scan:string(TXT), ok end))
  end || TXT<-TXTs ] , [ begin
  timer:tc(ntimes(5,fun () -> erlsom:simple_form(TXT), ok end)),
  timer:tc(ntimes(10,fun () -> erlsom:simple_form(TXT), ok end))
  end || TXT<-TXTs ] }
  .

ntimes(N,FUNC) -> fun () -> ntimes_(N,FUNC) end.
ntimes_(0,_) -> ok;
ntimes_(N,FUNC) -> FUNC(), ntimes_(N-1,FUNC).
