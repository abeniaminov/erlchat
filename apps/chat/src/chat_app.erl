%%%-------------------------------------------------------------------
%% @doc api public API
%% @end
%%%-------------------------------------------------------------------

-module(chat_app).

-behaviour(application).

-export([start/0, start/2, stop/1]).

-type cb_section() :: cowboy_http | cowboy_ws.

start() ->
    inets:start(),
    ok = application:start(crypto),
    ok = application:start(asn1),
    ok = application:start(public_key),
    ok = application:start(ssl),
    ok = application:start(ranch),
    ok = application:start(gproc),
    ok = application:start(cowlib),
    ok = application:start(cowboy),
    ok = application:start(chat).

start(_StartType, _StartArgs) ->
    {ok, Pid} = chat_sup:start_link(),
    chat_room_sup:start_room(<<"common_room">>),
    ok = start_cowboy(),
    {ok, Pid}.

stop(_State) ->
    ok.

%% internal functions
-spec start_cowboy() -> ok.
start_cowboy() ->
    HttpParams = get_cb_params(cowboy_http),
    HttpDispatch = cowboy_router:compile(http_routes()),
    {ok, _} = cowboy:start_clear(http, HttpParams, #{
        env => #{dispatch => HttpDispatch}
    }),

    WsParams =  get_cb_params(cowboy_ws),
    WsDispatch = cowboy_router:compile(ws_routes()),
    {ok, _} = cowboy:start_clear(ws, WsParams, #{
        env => #{dispatch => WsDispatch}
    }),
    ok.

-spec get_cb_params(cb_section()) -> map().
get_cb_params(Section) ->
    case application:get_env(chat, Section) of
        {ok, Config} when erlang:is_list(Config) ->
            NumAcceptors = proplists:get_value(nbacceptors, Config),
            IP = proplists:get_value(ip, Config),
            Port = proplists:get_value(http_port, Config),
            #{
                num_acceptors => NumAcceptors,
                socket_opts => [{ip, IP}, {port, Port}]
            };
        undefined ->
            throw("COWBOY SETTINGS NOT FOUND, CHECK YOUR CONFIG")
   end.

-spec http_routes() -> list().
http_routes()->
    [
        {'_',
            lists:flatten(
                lists:map(fun({U, H}) -> {U, H, []} end, api()),
                [
                    {"/", cowboy_static, {priv_file, chat, "index.html"}},
                    {"/static/[...]", cowboy_static, {priv_dir, chat, "static"}},
                    {'_', not_found_handler, []}
                ]
            )
        }
    ].

-spec ws_routes() -> list().
ws_routes() ->
    [
        {'_',
                [
                    {"/websocket", ws_chat_handler, []}
                ]
        }
    ].



-spec api() -> list().
api() ->
    [

    ].