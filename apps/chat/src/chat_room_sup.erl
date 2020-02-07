%%%-------------------------------------------------------------------
%%% @author abeniaminov
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. февр. 2020 09:52
%%%-------------------------------------------------------------------
-module(chat_room_sup).
-author("abeniaminov").

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([start_room/1]).
-export([get_active_room_names/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec start_room(binary()) -> binary().
start_room(Name) ->
    RoomId = uuid:uuid4(),
    {ok, _Pid } = supervisor:start_child(?MODULE, [RoomId, Name]),
    RoomId.

-spec get_active_room_names() -> list().
get_active_room_names() ->
    ChildList = supervisor:which_children(?SERVER),
    [#{<<"room_name">> => chat_room_worker:get_room_name(RoomPid)} || {_, RoomPid, _, _} <- ChildList].

%% @doc Starts the supervisor
-spec(start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
%% @doc Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
-spec(init(Args :: term()) ->
    {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
        MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
        [ChildSpec :: supervisor:child_spec()]}}
    | ignore | {error, Reason :: term()}).
init([]) ->
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 60,
    SupFlags = #{strategy => simple_one_for_one,
        intensity => MaxRestarts,
        period => MaxSecondsBetweenRestarts},

    Room = #{id => chat_room,
        start => {chat_room_worker, start_link, []},
        restart => temporary,
        shutdown => 2000,
        type => worker,
        modules => [chat_room_worker]},

    {ok, {SupFlags, [Room]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
