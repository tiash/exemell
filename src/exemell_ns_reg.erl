

-module(exemell_ns_reg).

-compile({no_auto_import,[get/0,get/1]}).
-include("exemell.hrl").
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3, format_status/2]).

-export([install/1, lookup/1, reload/0, get/0]).
-export([install/2, lookup/2, reload/1, get/1]).

-record(?MODULE,{reg}).
-define(reg(STATE),#?MODULE{reg=STATE}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

install(Namespace) ->
    case exemell_namespace:is(Namespace) of
      true -> gen_server:call(?MODULE,{install_,Namespace})
    end.

lookup(URI) when is_binary(URI); URI=:=none -> lookup_(URI,get());
lookup(NS) -> case exemell_namespace:is(NS) of true -> NS end.
reload() -> catch gen_server:call(?MODULE, reload), ok.
get() -> case catch get_server:call(?MODULE,get) of
          Reg=?reg(Dict) when element(1,Dict)=:=dict -> Reg;
          _ -> dict:new()
         end.


install(Namespace,Reg) ->
  case exemell_namespace:is(Namespace) of
    true -> install_(Namespace,Reg)
  end.

lookup(URI,Reg) when is_binary(URI); URI=:=none -> lookup_(URI,Reg);
lookup(NS,_Reg) -> case exemell_namespace:is(NS) of true -> NS end.
  
reload(Reg) ->
  lists:foldl(fun (NS,R) ->
    case exemell_namespace:is(NS) of
      true -> install_(NS,R);
      _ -> R
    end end, erlang:loaded(), Reg).

get(Reg) -> Reg.
  
init(_Args) -> {ok, reload(?reg(dict:new()))}.
handle_call({install,NS}, _From, Reg) ->
    {reply, ok, install(NS,Reg)};
handle_call({install_,NS}, _From, Reg) ->
    {reply, ok, install_(NS,Reg)};
handle_call(reload, _From, Reg) ->
    {reply, ok, reload(Reg)};
handle_call({lookup,NS}, _From, Reg) ->
    {reply, lookup(NS), Reg};
handle_call({lookup_,NS}, _From, Reg) ->
    {reply, lookup_(NS,Reg), Reg};
handle_call(get, _From, Reg) ->
    {reply, get(Reg), Reg}.
handle_cast(_Info,Reg) -> {noreply,Reg}.
handle_info(_Info,Reg) -> {noreply,Reg}.
terminate(_Reason,_State) -> ok.
code_change(_OVsn,State,_Extra) -> {ok,State}.
format_status(_Otp,[_PDict,?reg(Dict)]) -> [{data,[{"State",dict:to_list(Dict)}]}].

lookup_(URI,?reg(Dict)) ->
  case dict:find(URI,Dict) of
    {ok,NS} -> NS;
    error -> exemell_namespace:new(URI)
  end.

install_(NS,?reg(Dict)) -> ?reg(dict:store(NS:xmlns(),NS,Dict)).

