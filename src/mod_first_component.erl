-module(mod_first_component).

%% Required by ?INFO_MSG macrosls
-include("logger.hrl").
-include_lib("stdlib/include/qlc.hrl").

%% gen_mod API callbacks
-export([
	start/2, 
	stop/1,
	%get_domain_routes_pids/1,
	get_routes/0,
	get_pids_from_domain/1,
	get_pids_info/1,
	get_pids_sitems/1,
	get_pids_socket_data/1,
	get_pids_sockets/1,
	get_ips_ports_from_sockets/1]).

-ifndef(LAGER).
-define(LAGER, 1).
-endif.

-record(route, {domain, server_host, pid, local_hint}).
-record(pid_port, {pid, port}).

start(_Host, _Opt) -> 
    ?INFO_MSG("Hello, ejabberd world!", []),
    ok.

stop(_Host) -> 
    ?INFO_MSG("Bye bye, ejabberd world!", []),
    ok. 

get_routes() ->
    {atomic, Routes} = mnesia:transaction(fun() -> qlc:e(mnesia:table(route)) end),
    Routes.

get_pids_from_domain(Domain) -> 
	%% Avoid using dirty transactions unless you know the
	%% threading system of the program from the inside
	%% out. 
	%% Wrong: Routes = mnesia:dirty_read(route, Domain).
    {atomic, Pids} = mnesia:transaction(fun() -> qlc:e(qlc:q([X#route.pid || X <- mnesia:table(route), X#route.domain == Domain])) end),
    Pids.

get_pids_info(Pids) -> 
	Pids_Info = [{Pid, sys:get_status(Pid)} || Pid <- Pids],
	Pids_Info.

get_pid_sitem({Pid, Pids_Info}) ->
	case Pids_Info of
		{status, _, _, SItem} ->
			{Pid, SItem};
		_ ->
			?INFO_MSG('-p-n', [Pids_Info])
	end.

get_pids_sitems(Pids_Info) ->
	SItems = [get_pid_sitem(Pid_Info) || Pid_Info <- Pids_Info],
	SItems.

get_pid_socket_data({Pid, SItem}) ->
	case SItem of 
		[ _, _, _, _, [ _, _, {data, SocketData } ] ] ->
			{Pid, SocketData};
		_ ->
			?INFO_MSG('-p-n', [SItem])
	end.

get_pids_socket_data(SItems) ->
	SocketsData = [get_pid_socket_data(SItem) || SItem <- SItems],
	SocketsData.

get_pid_socket({Pid, SocketData}) ->
	case SocketData of 
		[ { _, { state, Socket, _, _, _, _, _, _ } } ] ->
			{Pid, Socket};
		_ ->
			none
	end.

get_pids_sockets(SocketsData) ->
	Sockets = [get_pid_socket(SocketData) || SocketData <- SocketsData],
	Sockets.

get_ip_port_from_socket({Pid, Socket}) ->
    IP_Port = ejabberd_socket:peername(Socket),
    {Pid, IP_Port}.

get_ips_ports_from_sockets(Sockets) ->
    IPs_Ports = [get_ip_port_from_socket(Socket) || Socket <- Sockets, (Socket /= none)],
    IPs_Ports.

% {_, _, _, Pid_data} = Pid_info.

% [_, _, _, _, Pid_int] = Pid_data.

% [_, _, Pid_status] = Pid_int.

% {_, Pid_state} = Pid_status. 

% [ { _, { _, Socket, _, _, _, _, _, _ } } ] = Pid_state.

% { _, _, Socket_port, Port_pid } = Socket.

% IP_Port = ejabberd_socket:peername(Socket).

% {_, { IP, Port }} = IP_Port.