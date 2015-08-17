%% Copyright 2014 Kolab Systems AG (http://www.kolabsys.com)
%%
%% Aaron Seigo (Kolab Systems) <seigo a kolabsys.com>
%%
%% This program is free software: you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%%
%% You should have received a copy of the GNU General Public License
%% along with this program.  If not, see <http://www.gnu.org/licenses/>.

-module(eimap).
-behaviour(gen_fsm).
-include("eimap.hrl").

%% API
-export([start_link/1, connect/1, disconnect/1, get_folder_annotations/4, get_message_headers_and_body/5, get_path_tokens/3]).

%% gen_fsm callbacks
-export([disconnected/2, authenticate/2, authenticating/2, idle/2, wait_response/2]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% state record definition
-record(state, { host, port, tls, user, pass, authed = false, socket,
                 command_serial = 1, command_queue = queue:new(),
                 current_command, current_mbox, parse_state }).
-record(command, { tag, mbox, message, from, response_token, parse_fun }).

%% public API
start_link(ServerConfig) when is_record(ServerConfig, eimap_server_config) -> gen_fsm:start_link(?MODULE, [ServerConfig], []).

connect(PID) -> gen_fsm:send_all_state_event(PID, connect).
disconnect(PID) -> gen_fsm:send_all_state_event(PID, disconnect).

get_folder_annotations(PID, From, ResponseToken, Folder) when is_list(Folder) ->
    get_folder_annotations(PID, From, ResponseToken, list_to_binary(Folder));
get_folder_annotations(PID, From, ResponseToken, Folder) when is_binary(Folder) ->
    Command = #command{ message = eimap_command_annotation:new(Folder),
                        from = From, response_token = ResponseToken,
                        parse_fun = fun eimap_command_annotation:parse/2 },
    gen_fsm:send_all_state_event(PID, { ready_command, Command }).

get_message_headers_and_body(PID, From, ResponseToken, Folder, MessageID) ->
    %%lager:info("SELECT_DEBUG: peeking message ~p ~p", [Folder, MessageID]),
    Command = #command{ mbox = Folder, message = eimap_command_peek_message:new(MessageID),
                        from = From, response_token = ResponseToken,
                        parse_fun = fun eimap_command_peek_message:parse/2 },
    gen_fsm:send_all_state_event(PID, { ready_command, Command }).

get_path_tokens(PID, From, ResponseToken) ->
    Command = #command{ message = eimap_command_namespace:new([]),
                        from = From, response_token = ResponseToken,
                        parse_fun = fun eimap_command_namespace:parse/2 },
    gen_fsm:send_all_state_event(PID, { ready_command, Command }).

%% gen_server API
init([#eimap_server_config{ host = Host, port = Port, tls = TLS, user = User, pass = Pass }]) ->
    State = #state {
                host = Host,
                port = Port,
                tls  = TLS,
                user = User,
                pass = Pass
              },
    { ok, disconnected, State }.

disconnected(connect, #state{ host = Host, port = Port, tls = TLS, socket = undefined } = State) ->
    {ok, Socket} = create_socket(Host, Port, TLS),
    { next_state, authenticate, State#state { socket = Socket } };
disconnected(Command, State) when is_record(Command, command) ->
    { next_state, disconnected, enque_command(Command, State) }.

authenticate({ data, _Data }, #state{ user = User, pass = Pass, authed = false } = State) ->
    Message = <<"LOGIN ", User/binary, " ", Pass/binary>>,
    Command = #command{ message = Message },
    send_command(Command, State),
    { next_state, authenticating, State#state{ authed = in_process } };
authenticate(Command, State) when is_record(Command, command) ->
    { next_state, authenticate, enque_command(Command, State) }.

authenticating({ data, Data }, #state{ authed = in_process } = State) ->
    Token = <<" OK ">>, %TODO would be nice to have the tag there
    case binary:match(Data, Token) of
        nomatch ->
            lager:warning("Log in to IMAP server failed: ~p", [Data]),
            close_socket(State),
            { next_state, idle, State#state{ socket = undefined, authed = false } };
        _ ->
            %%lager:info("Logged in to IMAP server successfully"),
            gen_fsm:send_event(self(), process_command_queue),
            { next_state, idle, State#state{ authed = true } }
    end;
authenticating(Command, State) when is_record(Command, command) ->
    { next_state, authenticating, enque_command(Command, State) }.

idle(process_command_queue, #state{ command_queue = Queue } = State) ->
    case queue:out(Queue) of
       { { value, Command }, ModifiedQueue } when is_record(Command, command) ->
            %%lager:info("Clearing queue of ~p", [Command]),
            StateWithNewQueue = State#state{ command_queue = ModifiedQueue },
            NewState = send_command(Command, StateWithNewQueue),
            { next_state, wait_response, NewState };
       { empty, ModifiedQueue } ->
            { next_state, idle, State#state{ command_queue = ModifiedQueue } }
    end;
idle({ data, _Data }, State) ->
    %%lager:info("Idling, server sent: ~p", [_Data]),
    { next_state, idle, State };
idle(Command, State) when is_record(Command, command) ->
    %%lager:info("Idling"),
    NewState = send_command(Command, State),
    { next_state, wait_response, NewState };
idle(_Event, State) ->
    { next_state, idle, State }.

%%TODO a variant that checks "#command{ from = undefined }" to avoid parsing responses which will go undelivered?
wait_response(Command, State) when is_record(Command, command) ->
    { next_state, wait_response, enque_command(Command, State) };
wait_response({ data, _Data }, #state{ current_command = #command{ parse_fun = undefined } } = State) ->
    gen_fsm:send_event(self(), process_command_queue),
    { next_state, idle, State };
wait_response({ data, Data }, #state{ current_command = #command{ parse_fun = Fun, tag = Tag } } = State) when is_function(Fun, 2) ->
    Response = Fun(Data, Tag),
    %%lager:info("Response from parser was ~p ~p, size of queue ~p", [More, Response, queue:len(State#state.command_queue)]),
    next_command_after_response(Response, State);
wait_response({ data, Data }, #state{ parse_state = ParseState, current_command = #command{ parse_fun = Fun, tag = Tag } } = State) when is_function(Fun, 3) ->
    Response = Fun(Data, Tag, ParseState),
    %%lager:info("Response from parser was ~p ~p, size of queue ~p", [More, Response, queue:len(State#state.command_queue)]),
    next_command_after_response(Response, State).

handle_event(connect, disconnected, State) ->
    gen_fsm:send_event(self(), connect),
    { next_state, disconnected, State };
handle_event(connect, _Statename, State) ->
    %%lager:info("Already connected to IMAP server!"),
    { next_state, _Statename, State };
handle_event(disconnect, _StateName, State) ->
    close_socket(State),
    { next_state, disconnected, reset_state(State) };
handle_event({ ready_command, Command }, StateName, State) when is_record(Command, command) ->
    %%lager:info("Making command .. ~p", [Command]),
    ?MODULE:StateName(Command, State);
handle_event(_Event, StateName, State) -> { next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) -> { next_state, StateName, State}.

handle_info({ ssl, Socket, Bin }, StateName, #state{ socket = Socket } = State) ->
    % Flow control: enable forwarding of next TCP message
    %lager:info("Received from server: ~p", [Bin]),
    ssl:setopts(Socket, [{ active, once }]),
    ?MODULE:StateName({ data, Bin }, State);
handle_info({ tcp, Socket, Bin }, StateName, #state{ socket = Socket } = State) ->
    % Flow control: enable forwarding of next TCP message
    %%lager:info("Got us ~p", [Bin]),
    inet:setopts(Socket, [{ active, once }]),
    ?MODULE:StateName({ data, Bin }, State);
handle_info({tcp_closed, Socket}, _StateName, #state{ socket = Socket, host = Host } = State) ->
    lager:info("~p Client ~p disconnected.\n", [self(), Host]),
    { stop, normal, State };
handle_info({ { selected, MBox }, ok }, StateName, State) ->
    %%lager:info("~p Selected mbox ~p", [self(), MBox]),
    { next_state, StateName, State#state{ current_mbox = MBox } };
handle_info({ { selected, MBox }, { error, Reason } }, StateName, State) ->
    lager:info("Failed to select mbox ~p: ~p", [MBox, Reason]),
    NewQueue = queue:filter(fun(Command) -> notify_of_mbox_failure_during_filter(Command, Command#command.mbox =:= MBox) end, State#state.command_queue),
    { next_state, StateName, State#state{ command_queue = NewQueue } };
handle_info(_Info, StateName, State) ->
    { next_state, StateName, State }.

terminate(_Reason, _Statename, State) -> close_socket(State), ok.

code_change(_OldVsn, Statename, State, _Extra) -> { ok, Statename, State }.

%% private API
notify_of_response(none, _Command) -> ok;
notify_of_response(_Response, #command { from = undefined }) -> ok;
notify_of_response(Response, #command { from = From, response_token = undefined }) -> From ! Response;
notify_of_response(Response, #command { from = From, response_token = Token }) -> From ! { Token, Response };
notify_of_response(_, _) -> ok.

%% the return is inverted for filtering
notify_of_mbox_failure_during_filter(Command, true) -> notify_of_response({ error, mailboxnotfound }, Command), false;
notify_of_mbox_failure_during_filter(_Command, false) -> true.

next_command_after_response({ more, Fun, ParseState }, State) when is_function(Fun, 3) ->
    { next_state, wait_response, State#state{ parse_state = ParseState, current_command = State#state.current_command#command{ parse_fun = Fun } } };
next_command_after_response({ more, ParseState }, State) ->
    { next_state, wait_response, State#state{ parse_state = ParseState } };
next_command_after_response({ error, _ } = ErrorResponse, State) ->
    notify_of_response(ErrorResponse, State#state.current_command),
    gen_fsm:send_event(self(), process_command_queue),
    { next_state, idle, State#state{ parse_state = none } };
next_command_after_response({ fini, Response }, State) ->
    notify_of_response(Response, State#state.current_command),
    gen_fsm:send_event(self(), process_command_queue),
    { next_state, idle, State#state{ parse_state = none } }.

tag_field_width(Serial) when Serial < 10000 -> 4;
tag_field_width(Serial) -> tag_field_width(Serial / 10000, 5).
tag_field_width(Serial, Count) when Serial < 10 -> Count;
tag_field_width(Serial, Count) -> tag_field_width(Serial / 10, Count + 1).

create_socket(Host, Port, true) -> ssl:connect(Host, Port, socket_options(), 1000);
create_socket(Host, Port, _) -> gen_tcp:connect(Host, Port, socket_options(), 1000).
socket_options() -> [binary, { active, once }, { send_timeout, 5000 }].

close_socket(#state{ socket = undefined }) -> ok;
close_socket(#state{ socket = Socket, tls = true }) -> ssl:close(Socket);
close_socket(#state{ socket = Socket }) -> gen_tcp:close(Socket).

reset_state(State) -> State#state{ socket = undefined, authed = false, command_serial = 1 }.

%% sending command code paths:
%%  0. not connected, TLS/SSL, unencrypted
%%  1. no mbox needed, mbox is already selected, mbox needs selecting
send_command(Command, #state{ socket = undefined } = State) ->
    lager:warning("Not connected, dropping command on floor: ~s", [Command]),
    State;
send_command(Command, #state{ tls = true} = State) ->
    send_command(fun ssl:send/2, Command, State);
send_command(Command, State) ->
    send_command(fun gen_tcp:send/2, Command, State).

send_command(Fun, #command{ mbox = undefined } = Command, State) ->
    %%lager:info("~p SELECT_DEBUG issuing command without mbox: ~p", [self(), Command#command.message]),
    send_command_now(Fun, Command, State);
send_command(Fun, #command{ mbox = MBox } = Command, #state{ current_mbox = CurrentMbox } = State) ->
    %%lager:info("~p SELECT_DEBUG issuing command with mbox ~p (current: ~p, equal -> ~p): ~p", [self(), MBox, CurrentMbox, (MBox =:= CurrentMbox), Command#command.message]),
    send_command_or_select_mbox(Fun, Command, State, MBox, MBox =:= CurrentMbox).

send_command_or_select_mbox(Fun, Command, State, _MBox, true) ->
    send_command_now(Fun, Command, State);
send_command_or_select_mbox(Fun, DelayedCommand, State, MBox, false) ->
    NextState = reenque_command(DelayedCommand, State),
    SelectMessage = eimap_command_examine:new(MBox),
    SelectCommand = #command{ message = SelectMessage, parse_fun = fun eimap_command_examine:parse/2,
                              from = self(), response_token = { selected, MBox } },
    %%lager:info("~p SELECT_DEBUG: Doing a select first ~p", [self(), SelectMessage]),
    send_command_now(Fun, SelectCommand, NextState).

send_command_now(Fun, #command{ message = Message } = Command, #state{ command_serial = Serial, socket = Socket } = State) ->
    Tag = list_to_binary(io_lib:format("EG~*..0B", [tag_field_width(Serial), Serial])),
    Data = <<Tag/binary, " ", Message/binary, "\n">>,
    %%lager:info("Sending command via ~p: ~s", [Fun, Data]),
    Fun(Socket, Data),
    State#state{ command_serial = Serial + 1, current_command = Command#command{ tag = Tag } }.

enque_command(Command, State) ->
    %%lager:info("Enqueuing command ~p", [Command]),
    State#state { command_queue = queue:in(Command, State#state.command_queue) }.

reenque_command(Command, State) ->
    %%lager:info("Re-queueing command ~p", [Command]),
    State#state { command_queue = queue:in_r(Command, State#state.command_queue) }.

