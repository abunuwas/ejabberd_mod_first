-module(mod_first_component).

%% Required by ?INFO_MSG macrosls
-include("logger.hrl").

%% gen_mod API callbacks
-export([
	start/2, 
	stop/1,
	get_routes/0,
	get_routes_of_same_subdomain/1,
	get_pids_from_routes/1,
	get_ips_ports_from_pids/1]).

-ifndef(LAGER).
-define(LAGER, 1).
-endif.

start(_Host, _Opt) -> 
    ?INFO_MSG("Hello, ejabberd world!", []),
    ok.

stop(_Host) -> 
    ?INFO_MSG("Bye bye, ejabberd world!", []),
    ok. 

get_routes() ->
    {atomic, Routes} = mnesia:transaction(fun() -> qlc:e(mnesia:table(route)) end),
    Routes.

get_routes_of_same_subdomain(Subdomain) ->
	Routes = get_routes(),
    Host_Routes = [Route || Route <- Routes, (get_subdomain(Route) == Subdomain)],
    Host_Routes.

get_subdomain(Record) ->
	{_, Subdomain, _, _, _} = Record,
	Subdomain.

get_pids_from_routes(Routes) ->
    Pids = [erlang:element(4, Route) || Route <- Routes],
    Pids.

get_pids_info(Pids) -> 
	Pids_Info = [sys:get_status(Pid) || Pid <- Pids],
	Pids_Info.

get_pid_state(Pid) ->
    Pid_Data = erlang:element(4, Pid),
    Pid_Status = erlang:element(5, Pid_Data),
    {data, Pid_State_Data} = erlang:element(3, Pid_Status),
    Pid_State_Data.

get_pids_state(Pids_Info) ->
    Pids_State = [get_pid_state(Pid) || Pid <- Pids_Info],
    Pids_State.

get_pid_socket(Pid_State) ->
    [ { _, { _, Socket, _, _, _, _, _, _ } } ] = Pid_State,
    Socket.

get_ip_port(Socket) ->
    IP_Port = ejabberd_socket:peername(Socket),
    IP_Port.

get_ips_ports_from_pids(Pids) ->
    Pids_State = get_pids_state(get_pids_info(Pids)),
    Sockets = [get_pid_socket(Pid_State) || Pid_State <- Pids_State],
    IPs_Ports = [get_ip_port(Socket) || Socket <- Sockets],
    IPs_Ports.



% {_, _, _, Pid_data} = Pid_info.

% [_, _, _, _, Pid_int] = Pid_data.

% [_, _, Pid_status] = Pid_int.

% {_, Pid_state} = Pid_status. 

% [ { _, { _, Socket, _, _, _, _, _, _ } } ] = Pid_state.

% { _, _, Socket_port, Port_pid } = Socket.

% IP_Port = ejabberd_socket:peername(Socket).

% {_, { IP, Port }} = IP_Port.