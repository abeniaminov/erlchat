%%%-------------------------------------------------------------------
%%% @author abeniaminov
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. февр. 2020 13:40
%%%-------------------------------------------------------------------
-author("abeniaminov").
-type(room_id() :: binary()).

-record(room, {
    id :: room_id(),
    name :: binary(),
    user_pids = [] :: orddict:orddict(),
    history = [] ::list()
}).