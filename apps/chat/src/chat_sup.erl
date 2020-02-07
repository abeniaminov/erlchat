%%%-------------------------------------------------------------------
%% @doc api top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(chat_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one,
        intensity => 5,
        period => 10},

    RoomSup = {chat_room_sup, {chat_room_sup, start_link, []},
        permanent, 5000, supervisor, dynamic},

    ChildSpecs = [RoomSup],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
