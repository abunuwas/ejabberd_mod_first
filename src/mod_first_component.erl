%%%----------------------------------------------------------------------
%%% File    : mod_first_comopnent.erl
%%% Author  : Jose Haro Peralta <jharoperalta@intamac.com>
%%% Purpose : Query preexisting Component connections. 
%%% Created : 22 Aug 2016 by Jose Haro Peralta <jharoperalta@intamac.com>
%%%
%%% ----------------------------------------------------------------------
%%% Description: 
%%% This module implements a hook on the event component_connected
%%% raised by Ejabberd whenever a new Component is registered under
%%% a specific route. The event is raised inmediately after the route
%%% has been recorded in mnesia. It therefore opens the door for the
%%% possibility of querying for previously connected Components under
%%% the connecting Component's domain. 
%%% If the number of Pids registered under the connecting Component's
%%% domain is 1, assume that this is the Pid of the connecting Component,
%%% and therefore that this is the first Component starting a session
%%% under the domain. That means that no Component was previously 
%%% registered under that domain, and therefore we need to send
%%% presence stanzas to the Component's domain to notify the platform
%%% of the current status of devices. 
%%% ----------------------------------------------------------------------
%%%
%%% Copyright (C) 2002-2016   Intamac Systems Ltd.
%%%
%%% Copy right statement... .
%%%
%%%----------------------------------------------------------------------

-module(mod_first_component).

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
	start/2, 
	stop/1,
	%get_domain_routes_pids/1,
	get_pids_from_domain/1,
	send_presences/1,
	component_connected_hook/1
	]).


%% Some dependency is using Lager for logging. 
%% Sometimes if Lager is not declared with a 
%% specific logging level, an error is thrown. 
%% Therefore the following statements. 
-ifndef(LAGER).
-define(LAGER, 1).
-endif.


%% Define record for mnesia table route. 
-record(route, {domain, server_host, pid, local_hint}).


%% Register function component_connected_hook to be 
%% added to the list of callbacks to be invoked when 
%% event `component_connected` is raised by Ejabberd. 
start(_Host, _Opt) -> 
    ejabberd_hooks:add(component_connected, global, ?MODULE, component_connected_hook, 50),
    ?INFO_MSG("Registering component_connected_hook...", []),
    ok.


%% Deregister the function component_connected_hook 
%% when Ejabberd disconnects. 
stop(_Host) -> 
    ejabberd_hooks:delete(component_connected, _Host, ?MODULE, component_connected_hook, 50),
	?INFO_MSG("Deregistering component_connected_hook...", []),
    ok. 


%% Returns a list of Pids associated with a domain
%% from mnesia table route. 
get_pids_from_domain(Domain) -> 
	%% Avoid using dirty transactions unless you know the
	%% threading system of the program from the inside
	%% out. 
	%% Wrong: Routes = mnesia:dirty_read(route, Domain).
    {atomic, Pids} = mnesia:transaction(fun() -> qlc:e(qlc:q([X#route.pid || X <- mnesia:table(route), X#route.domain == Domain])) end),
    Pids.


%% Routes `presence` stanzas of type `available` on 
%% behalf of connected users to a domain H. 
send_presences(H) ->
	Sessions = ejabberd_sm:dirty_get_sessions_list(),
	To = jid:make(<<>>, H, <<>>),
	%% The following presence packet contains a special msg
	%% attribute to distinguish it from presence packets
	%% sent by users in normal circumstances. Remove? 
	Packet = {xmlel,
				<<"presence">>,
				[{<<"from">>,<<"user1@localhost">>},{<<"to">>,<<"muc.localhost">>},{<<"type">>,<<"available">>},{<<"msg">>,<<"session-started">>}],
				[]
				},
	lists:foreach(
		fun({U, S, R}) ->
			From = jid:make(U, S, R),
			ejabberd_router:route(From, To, Packet)
		end, Sessions).

%% On event component_connected, query mnesia table route
%% to obtain a list of Pids registered under the connecting
%% Component's domain. If number of Pids is 1, send presence
%% stanzas on behalf of connected users to the connecting
%% Component's domain. 
component_connected_hook(H) ->
    ?INFO_MSG("mod_first_comopnent:component_connected_hook [116]: A Component was connected with domain: ~p~n", [H]),
    Components = mod_first_component:get_pids_from_domain(H),
    ?INFO_MSG("mod_first_comopnent:component_connected_hook [118]: Pids associated with the connecting domain: ~p~n", [Components]),
    NumComponents = length(Components),
    case NumComponents of
    	1 ->
    	?INFO_MSG("mod_first_comopnent:component_connected_hook [122]: A Component is starting session for domain: ~p~n", [H]),
    		send_presences(H);
    	_ ->
    		?INFO_MSG("mod_first_comopnent:component_connected_hook [125]: Not the first Component registering under this domain, keep going on.", [])
    	end,
    ok.
