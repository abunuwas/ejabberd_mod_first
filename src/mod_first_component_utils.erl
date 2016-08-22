%%%----------------------------------------------------------------------
%%% File    : mod_first_comopnent_utils.erl
%%% Author  : Jose Haro Peralta <jharoperalta@intamac.com>
%%% Purpose : Provides utils for querying preexisting Component connections. 
%%% Created : 22 Aug 2016 by Jose Haro Peralta <jharoperalta@intamac.com>
%%%
%%% Copyright (C) 2002-2016   Intamac Systems Ltd.
%%%
%%% Copy right statement... .
%%%
%%%----------------------------------------------------------------------

-module(mod_first_component_utils).

-author("Jose Haro Peralta").
-craeted("Date: 2016/08/22"). 

%% Load records definitions.
-include("ejabberd.hrl").
-include("jlib.hrl").

%% This one is required by ?INFO_MSG macros.
-include("logger.hrl").

%% Include records definitions from qlc for
%% use in mnesia function-based queries.
-include_lib("stdlib/include/qlc.hrl").

%% Public functions: 
-export([
    get_routes/0,
    get_pids_info/1,
    get_pids_sitems/1,
    get_pids_socket_data/1,
    get_pids_sockets/1,
    get_ips_ports_from_sockets/1,
    get_list_pids_ports/1]).

%% Some dependency is using Lager for 
%% logging. Sometimes if Lager is not 
%% declared with a specific logging level, 
%% an error is thrown. Therefore the 
%% following statements. 
-ifndef(LAGER).
-define(LAGER, 1).
-endif.

%% Define record for mnesia table route. 
-record(route, {domain, server_host, pid, local_hint}).


%% Get all records from mnesia table route. 
get_routes() ->
    {atomic, Routes} = mnesia:transaction(fun() -> qlc:e(mnesia:table(route)) end),
    Routes.

%% Run sys call on the Pid to get status info. 
get_pids_info(Pids) -> 
    Pids_Info = [{Pid, sys:get_status(Pid)} || Pid <- Pids],
    Pids_Info.

%% Fetch the last element from the tuple returned
%% by sys:get_status, named SItem. 
get_pid_sitem({Pid, Pids_Info}) ->
    case Pids_Info of
        {status, _, _, SItem} ->
            {Pid, SItem};
        _ ->
            ?INFO_MSG('-p-n', [Pids_Info])
    end.

%% Apply get_pid_sitem to a list of pids.
get_pids_sitems(Pids_Info) ->
    SItems = [get_pid_sitem(Pid_Info) || Pid_Info <- Pids_Info],
    SItems.

%% Fetch socket data from SItem element. 
get_pid_socket_data({Pid, SItem}) ->
    case SItem of 
        [ _, _, _, _, [ _, _, {data, SocketData } ] ] ->
            {Pid, SocketData};
        _ ->
            ?INFO_MSG('-p-n', [SItem])
    end.

%% Apply get_pid_socket_data to a list of SItems. 
get_pids_socket_data(SItems) ->
    SocketsData = [get_pid_socket_data(SItem) || SItem <- SItems],
    SocketsData.

%% Get specific socket data. 
get_pid_socket({Pid, SocketData}) ->
    case SocketData of 
        [ { _, { state, Socket, _, _, _, _, _, _ } } ] ->
            {Pid, Socket};
        _ ->
            none
    end.

%% Apply get_pid_socket to a list of elements.
get_pids_sockets(SocketsData) ->
    Sockets = [get_pid_socket(SocketData) || SocketData <- SocketsData],
    Sockets.

%% Invoke ejabberd_socket:peername passing in
%% socket data to obtain IP and Port associated
%% with each socket. 
get_ip_port_from_socket({Pid, Socket}) ->
    IP_Port = ejabberd_socket:peername(Socket),
    {Pid, IP_Port}.

%% Apply get_ip_port_from_socket to a list of
%% socket data tuples. 
get_ips_ports_from_sockets(Sockets) ->
    IPs_Ports = [get_ip_port_from_socket(Socket) || Socket <- Sockets, (Socket /= none)],
    IPs_Ports.

%% Put eveything together to obtain a list of tuples
%% describing the binding from pid to IP+Port for a 
%% given domain. 
get_list_pids_ports(Domain) ->
    Pids = mod_first_component:get_pids_from_domain(Domain),
    Pids_Infos = get_pids_info(Pids),
    SItems = get_pids_sitems(Pids_Infos),
    SocketsData = get_pids_socket_data(SItems),
    Sockets = get_pids_sockets(SocketsData),
    Ips_Ports = get_ips_ports_from_sockets(Sockets),
    Ips_Ports.
