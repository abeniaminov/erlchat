%%%-------------------------------------------------------------------
%%% @author abeniaminov
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. февр. 2020 19:03
%%%-------------------------------------------------------------------
-module(utl).
-author("abeniaminov").

%% API
-export([offset_limit/3]).
-export([send_messages_to_users/3]).

offset_limit([], _Limit, _Offset) -> [];
offset_limit(LSort, Limit, Offset) ->
    case length(LSort) > Offset of
        true ->
            {_L1, L2} = lists:split(Offset, LSort),
            case length(L2) > Limit of
                true ->
                    {L3, _L4} = lists:split(Limit, L2),
                    L3;
                false ->
                    L2

            end;
        false ->
            []
    end.

send_messages_to_users(Function, UserPids, MessageList) ->
    Json = jsx:encode(#{<<"function">> => Function, <<"msgs">>  => MessageList}),
    send_broadcast(UserPids, {messages, Json}).

send_broadcast([], _Msg) -> ok;
send_broadcast([H|T], Msg) ->
    H ! Msg,
    send_broadcast(T, Msg).
