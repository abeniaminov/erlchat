%%%-------------------------------------------------------------------
%%% @author abeniaminov
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. февр. 2020 09:53
%%%-------------------------------------------------------------------
-module(chat_room_worker).
-author("abeniaminov").

-behaviour(gen_server).

-include("chat.hrl").
-include_lib("kernel/include/logger.hrl").

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-export([get_room_name/1]).
-export([join/3]).
-export([leave/2]).
-export([new_message/4]).

-export([leave_cast/2]).
-export([new_message_cast/4]).

-define(SERVER, ?MODULE).
-define(LIMIT, 1000).


%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server
-spec(start_link(binary(), binary()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(RoomId, Name) ->
    gen_server:start_link(?MODULE, [RoomId, Name], []).

get_room_name(RoomPid) ->
    do_call(RoomPid, {get_room_name}).

join(RoomName, UserPid, UserName) ->
    do_call(RoomName, {join, UserPid, UserName}).


leave(RoomName, UserPid) ->
    do_call(RoomName, {leave, UserPid}).

leave_cast(RoomName, UserPid) ->
    do_cast(RoomName, {leave, UserPid}).

new_message(RoomName, UserPid, UserName, Message) ->
    do_call(RoomName, {new_message, UserPid, UserName, Message}).

new_message_cast(RoomName, UserPid, UserName, Message) ->
    do_cast(RoomName, {new_message, UserPid, UserName, Message}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
    {ok, State :: #room{}} | {ok, State :: #room{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([RoomId, Name]) ->
    gproc:add_local_name({room_name, Name}),
    {ok, #room{id = RoomId, name = Name, user_pids = [], history = []}}.

handle_call({get_room_name}, _From,
    State = #room{name = Name}) ->
    {reply, Name, State};

handle_call({join, UserPid, UserName}, _From,
    State = #room{
        user_pids = Pids,
        history = History}) ->
    Ref = erlang:monitor(process, UserPid),
    Pids1 = orddict:append(UserPid, {UserName, Ref}, Pids),
    HelloMessage = #{<<"user_name">> => UserName, <<"message">> => <<"Привет !"/utf8>>},
    NewHistory = [HelloMessage | History],
    MessageList = lists:reverse(get_messages_from_history(History, ?LIMIT, 0)),
    utl:send_messages_to_users(<<"show_message">>,[UserPid], MessageList),
    UserPids = orddict:fetch_keys(Pids1),
    utl:send_messages_to_users(<<"show_message">>,UserPids, [HelloMessage]),
    {reply, ok, State#room{user_pids = Pids1, history = NewHistory}};

handle_call({leave, UserPid}, _From,
    State = #room{
        user_pids = Pids}) ->
    case orddict:take(UserPid, Pids) of
        error -> continue;
        {_,Ref} ->
            erlang:demonitor(Ref,[flush])
    end,
    {reply, ok, State};

handle_call({new_message, UserPid, UserName, Message}, _From,
    State = #room{
        user_pids = Pids,
        history = History}) ->
    DMessage = #{<<"user_name">> => UserName, <<"message">> => Message},
    NewState =
        case orddict:is_key(UserPid, Pids) of
            true ->
                NewHistory = [DMessage| History],
                UserPids = orddict:fetch_keys(Pids),
                utl:send_messages_to_users(<<"show_message">>,UserPids, [DMessage]),
                State#room{history = NewHistory};
            false ->
                State
        end,
    {reply, ok, NewState} .


handle_cast({leave, UserPid},
    State = #room{
        user_pids = Pids}) ->
    case orddict:take(UserPid, Pids) of
        error -> continue;
        {_,Ref} ->
            erlang:demonitor(Ref,[flush])
    end,
    {noreply, State};

handle_cast({new_message, UserPid, UserName, Message},
    State = #room{
        user_pids = Pids,
        history = History}) ->
    DMessage = #{<<"user_name">> => UserName, <<"message">> => Message},
    NewState =
        case orddict:is_key(UserPid, Pids) of
            true ->
                NewHistory = [DMessage| History],
                UserPids = orddict:fetch_keys(Pids),
                utl:send_messages_to_users(<<"show_message">>, UserPids, [DMessage]),
                State#room{history = NewHistory};
            false ->
                State
        end,
    {noreply, NewState};

handle_cast(_Request, State = #room{}) ->
    {noreply, State}.

handle_info({'DOWN', MonitorRef, process, Pid, _Info}, State = #room{user_pids = Pids}) ->
    logger:notice("process ~p DOWN ", [Pid]),
    NewPids =
        case orddict:find(Pid, Pids) of
            error ->
                Pids;
            {ok, {_, MonitorRef}} ->
                logger:notice("process ~p erased from State", [Pid]),
                orddict:erase(Pid, Pids);
            {ok, _} ->
                Pids
        end,
    {noreply, State#room{user_pids = NewPids}}.

terminate(_Reason, _State = #room{}) ->
    ok.

code_change(_OldVsn, State = #room{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

do_call(RoomName, Request) when is_binary(RoomName) ->
    do_call_1(get_pid(RoomName), Request);
do_call(RoomPid, Request) when is_pid(RoomPid) ->
    do_call_1(RoomPid, Request).

do_call_1(undefined, _Request) ->
    logger:error("room_not_found~n", []),
    {error, room_not_found};
do_call_1(Pid, Request) ->
    gen_server:call(Pid, Request).

do_cast(RoomName, Request) when is_binary(RoomName) ->
    do_cast_1(get_pid(RoomName), Request);
do_cast(RoomPid, Request) when is_pid(RoomPid) ->
    do_cast_1(RoomPid, Request).

do_cast_1(Pid, Request) ->
    gen_server:cast(Pid, Request).

get_pid(RoomName) ->
    gproc:lookup_local_name({room_name, RoomName}).

get_messages_from_history(History, Limit, Offset) ->
    utl:offset_limit(History, Limit, Offset).


