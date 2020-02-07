-module(ws_chat_handler).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

-define(IDLE_TIMEOUT, 600000).
-define(SET_OPTIONS, {set_options, #{idle_timeout => ?IDLE_TIMEOUT }}).

init(Req, State) ->
	{cowboy_websocket, Req, State, #{idle_timeout => ?IDLE_TIMEOUT }}.

websocket_init(State) ->
	{[], State}.

websocket_handle({text, Msg}, State) ->
	logger:notice("Msg: ~ts", [Msg]),
	MsgMap = jsx:decode(Msg, [return_maps]),
	do_command(MsgMap),
	{ok, State, hibernate};
websocket_handle(_Data, State) ->
	{ok, State, hibernate}.

websocket_info({messages, Msg}, State) ->
	{[{text, Msg}], State, hibernate};
websocket_info(_Info, State) ->
	{ok, State}.


do_command(#{<<"command">> := <<"get_active_rooms">>}) ->
	ActiveRooms = chat_room_sup:get_active_room_names(),
    utl:send_messages_to_users(<<"active_rooms">>, [self()], ActiveRooms);

do_command(#{<<"command">> := <<"join">>} = MsgMap) ->
	chat_room_worker:join(maps:get(<<"room">>, MsgMap), self(),
		maps:get(<<"user_name">>, MsgMap));

do_command(#{<<"command">> := <<"new_message">>} = MsgMap) ->
	chat_room_worker:new_message_cast(maps:get(<<"room">>, MsgMap), self(),
		maps:get(<<"user_name">>, MsgMap),
		maps:get(<<"message">>, MsgMap, <<"">>));

do_command(_) -> ok.
