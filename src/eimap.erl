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
-export([start_link/1,
         %% passthrough mode, where data is just sent to the server blindly and
         %% responses passed back equally blindly. in this mode the user is on
         %% their own and better know what they are doing. can only be activated
         %% when disconnected or idle
         start_passthrough/2, stop_passthrough/1, passthrough_data/2,
         %% connection management
         connect/1, connect/3, disconnect/1,
         %% commands
         starttls/3,
         capabilities/3,
         login/5, logout/3,
         compress/1,
         get_server_metadata/4,
         get_folder_status/5,
         get_folder_metadata/5,
         get_folder_annotations/4,
         get_message_headers_and_body/5,
         get_path_tokens/3]).

%% gen_fsm callbacks
-export([disconnected/2, idle/2, passthrough/2, wait_response/2, startingtls/2]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% state record definition
-record(state, { host, port, tls, tls_state = false, socket, server_id = <<>>,
                 command_serial = 1, command_queue = queue:new(),
                 current_command, current_mbox,
                 passthrough = false, passthrough_recv, passthrough_send_buffer = <<>>,
                 inflator, deflator}).
-record(command, { tag, mbox, message, from, response_type, response_token, parse_state }).

-define(SSL_UPGRADE_TIMEOUT, 5000).
-define(TCP_CONNECT_TIMEOUT, 5000).

%% public API
start_link(ServerConfig) when is_record(ServerConfig, eimap_server_config) -> gen_fsm:start_link(?MODULE, ServerConfig, []).

start_passthrough(PID, Receiver) when is_pid(Receiver)  -> gen_fsm:send_event(PID, { start_passthrough, Receiver } ).
stop_passthrough(PID) -> gen_fsm:send_event(PID, stop_passthrough).
passthrough_data(PID, Data) when is_binary(Data) -> gen_fsm:send_all_state_event(PID, { passthrough, Data }).
connect(PID) -> connect(PID, undefined, undefined).
connect(PID, From, ResponseToken) -> gen_fsm:send_all_state_event(PID, { connect, From, ResponseToken }).
disconnect(PID) -> gen_fsm:send_all_state_event(PID, disconnect).

compress(PID) when is_pid(PID) ->
    Command = #command{ message = eimap_command_compress:new(ok),
                        from = PID, response_token = compress,
                        parse_fun = fun eimap_command_compress:parse/2 },
    gen_fsm:send_all_state_event(PID, { ready_command, Command }).

starttls(PID, From, ResponseToken) when is_pid(PID) ->
    Command = #command{ message = eimap_command_starttls:new(ok),
                        from = From, response_token = ResponseToken,
                        parse_fun = fun eimap_command_starttls:parse/2 },
    gen_fsm:send_all_state_event(PID, { ready_command, Command }).

capabilities(PID, From, ResponseToken) when is_pid(PID) ->
    Command = #command{ message = eimap_command_capability:new(noparams),
                        from = From, response_token = ResponseToken,
                        parse_fun = fun eimap_command_capability:parse/2 },
    gen_fsm:send_all_state_event(PID, { ready_command, Command }).

-spec login(PID :: pid(), From :: pid(), ResponseToken :: any(), User :: list() | binary(), Pass :: list() | binary()) -> ok.
login(PID, From, ResponseToken, User, Pass) ->
    Command = #command{ message = eimap_command_login:new({ User, Pass }),
                        from = From, response_token = ResponseToken,
                        parse_fun = fun eimap_command_login:parse/2 },
    gen_fsm:send_all_state_event(PID, { ready_command, Command }).

-spec logout(PID :: pid(), From :: pid(), ResponseToken :: any()) -> ok.
logout(PID, From, ResponseToken) ->
    Command = #command{ message = eimap_command_logout:new(ok),
                        from = From, response_token = ResponseToken,
                        parse_fun = fun eimap_command_logout:parse/2 },
    gen_fsm:send_all_state_event(PID, { ready_command, Command }).

-type status_property() :: messages | recent | uidnext | uidvalidity | unseen.
-type status_properties() :: [status_property()].
-spec get_folder_status(PID :: pid(), From :: pid(), ResponseToken :: any(), Folder :: list() | binary(), Properties:: status_properties()) -> ok.
get_folder_status(PID, From, ResponseToken, Folder, Properties) ->
    Command = #command{ message = eimap_command_status:new({ Folder, Properties }),
                        from = From, response_token = ResponseToken,
                        parse_fun = fun eimap_command_status:parse/2 },
    gen_fsm:send_all_state_event(PID, { ready_command, Command }).

-spec get_folder_metadata(PID :: pid(), From :: pid(), ResponseToken :: any(), Folder :: list() | binary(), Properties:: [list() | binary()]) -> ok.
get_folder_metadata(PID, From, ResponseToken, Folder, Properties) ->
    Command = #command{ message = eimap_command_getmetadata:new({ Folder, Properties}),
                        from = From, response_token = ResponseToken,
                        parse_fun = fun eimap_command_getmetadata:parse/2 },
    gen_fsm:send_all_state_event(PID, { ready_command, Command }).

-spec get_server_metadata(PID :: pid(), From :: pid(), ResponseToken :: any(), Properties:: [list() | binary()]) -> ok.
get_server_metadata(PID, From, ResponseToken, Properties) ->
    Command = #command{ message = eimap_command_getmetadata:new({ <<>>, Properties}),
                        from = From, response_token = ResponseToken,
                        parse_fun = fun eimap_command_getmetadata:parse/2 },
    gen_fsm:send_all_state_event(PID, { ready_command, Command }).

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
init(#eimap_server_config{ host = Host, port = Port, tls = TLS }) ->
    State = #state { host = Host, port = Port, tls  = TLS },
    { ok, disconnected, State }.

disconnected({ start_passthrough, Receiver }, State) ->
    { next_state, disconnected, State#state{ passthrough = true, passthrough_recv = Receiver } };
disconnected(stop_passthrough, State) ->
    { next_state, disconnected, State#state{ passthrough = false } };
disconnected({ connect, Receiver, ResponseToken }, #state{ command_queue = CommandQueue, host = Host, port = Port, tls = TLS, socket = undefined } = State) ->
    %lager:debug("CONNECTING! ~p ~p", [Receiver, ResponseToken]),
    { {ok, Socket}, TlsState, SendCapabilitiesTo, NewCommandQueue } = create_socket(Host, Port, TLS, Receiver, ResponseToken, CommandQueue),
    Command = #command{ message = eimap_command_capability:new([]),
                        from = SendCapabilitiesTo, response_token = { connected, Receiver, ResponseToken },
                        parse_fun = fun eimap_command_capability:parse/2 },
    { next_state, wait_response, State#state { socket = Socket, tls_state = TlsState, current_command = Command, command_queue = NewCommandQueue } };
disconnected(Command, State) when is_record(Command, command) ->
    { next_state, disconnected, enque_command(Command, State) }.

passthrough(flush_passthrough_buffer, #state{ passthrough_send_buffer = Buffer } = State) ->
    %lager:info("Passing through ~p", [Buffer]),
    passthrough({ passthrough, Buffer }, State#state{ passthrough_send_buffer = <<>> });
passthrough({ passthrough, Data }, #state{ socket = Socket, tls_state = true } = State) ->
    %lager:info("Passing through ssl \"~s\"", [Data]),
    ssl:send(Socket, deflated(Data, State)),
    { next_state, passthrough, State };
passthrough({ passthrough, Data }, #state{ socket = Socket } = State) ->
    %lager:info("Passing through tcp \"~s\"", [Data]),
    gen_tcp:send(Socket, deflated(Data, State)),
    { next_state, passthrough, State };
passthrough({ data, Data }, #state{ passthrough_recv = Receiver } = State) ->
    %lager:info("Passing back ~p", [Data]),
    Receiver ! { imap_server_response, Data },
    { next_state, passthrough, State };
passthrough({ start_passthrough, Receiver }, State) ->
    %% already in passthrough, but perhaps the Receiver changes ...
    lager:warning("Already in passthrough mode, and passthrough mode was requested again!"),
    %% TODO: should this count the # of times start is called and require an equal # of ends?
    { next_state, passthrough, State#state{ passthrough_recv = Receiver } };
passthrough(stop_passthrough, State) ->
    gen_fsm:send_event(self(), process_command_queue),
    { next_state, idle, State#state{ passthrough = false } };
passthrough(Command, State) when is_record(Command, command) ->
    gen_fsm:send_event(self(), process_command_queue),
    { next_state, idle, enque_command(Command, State) }.

idle(process_command_queue, #state{ command_queue = Queue } = State) ->
    case queue:out(Queue) of
       { { value, Command }, ModifiedQueue } when is_record(Command, command) ->
            %%lager:info("Clearing queue of ~p", [Command]),
            NewState = send_command(Command, State#state{ command_queue = ModifiedQueue }),
            { next_state, wait_response, NewState };
       { empty, ModifiedQueue } ->
            NextState = next_state_after_emptied_queue(State),
            { next_state, NextState, State#state{ command_queue = ModifiedQueue } }
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

next_state_after_emptied_queue(#state{ passthrough = true }) ->
    gen_fsm:send_event(self(), flush_passthrough_buffer),
    passthrough;
next_state_after_emptied_queue(_State) ->
    idle.


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

startingtls({ passthrough, Data }, #state{ passthrough = true, passthrough_send_buffer = Buffer } = State) ->
    { next_state, startingtls, State#state{ passthrough_send_buffer = <<Buffer/binary, Data>> } };
startingtls(Command, State) when is_record(Command, command) ->
    { next_state, startingtls, enque_command(Command, State) };
startingtls({ data, Data }, #state{ current_command = #command{ parse_fun = Fun, tag = Tag } } = State) when is_function(Fun, 2) ->
    Response = Fun(Data, Tag),
    %%lager:info("Response from parser was ~p ~p, size of queue ~p", [More, Response, queue:len(State#state.command_queue)]),
    next_command_after_response(Response, State).

handle_event({ connect, _From, _ResponseToken } = Event, disconnected, State) ->
    gen_fsm:send_event(self(), Event),
    { next_state, disconnected, State };
handle_event({ connect, _From, _ResponseToken }, _Statename, State) ->
    %%lager:info("Already connected to IMAP server!"),
    { next_state, _Statename, State };
handle_event(disconnect, _StateName, State) ->
    close_socket(State),
    { next_state, disconnected, reset_state(State) };
handle_event({ ready_command, Command }, StateName, State) when is_record(Command, command) ->
    ?MODULE:StateName(Command, State);
handle_event({ passthrough, Data }, passthrough, #state{ passthrough = true } = State) ->
    ?MODULE:passthrough({ passthrough, Data }, State);
handle_event({ passthrough, Data }, StateName, #state{ passthrough = true, passthrough_send_buffer = Buffer } = State) ->
    NewBuffer = <<Buffer/binary, Data/binary>>,
    { next_state, StateName, State#state{ passthrough_send_buffer = NewBuffer } };
handle_event(_Event, StateName, State) -> { next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) -> { next_state, StateName, State}.

handle_info({ ssl, Socket, Bin }, StateName, #state{ socket = Socket } = State) ->
    % Flow control: enable forwarding of next TCP message
    ssl:setopts(Socket, [{ active, once }]),
    Data = inflated(Bin, State),
    %lager:info("Received from server over ssl: ~s", [Data]),
    ?MODULE:StateName({ data, Data }, State);
handle_info({ tcp, Socket, Bin }, StateName, #state{ socket = Socket } = State) ->
    % Flow control: enable forwarding of next TCP message
    inet:setopts(Socket, [{ active, once }]),
    Data = inflated(Bin, State),
    %lager:info("Received from server plaintext: ~s", [Data]),
    ?MODULE:StateName({ data, Data }, State);
handle_info({ ssl_closed, Socket }, _StateName, #state{ socket = Socket, host = Host, port = Port } = State) ->
    lager:info("~p Disconnected from ~p:~p .\n", [self(), Host, Port]),
    { stop, normal, State };
handle_info({ ssl_error, Socket, _Reason }, _StateName, #state{ socket = Socket, host = Host, port = Port } = State) ->
    lager:info("~p Disconnected due to socket error from ~p:~p .\n", [self(), Host, Port]),
    { stop, normal, State };
handle_info({ tcp_closed, Socket }, _StateName, #state{ socket = Socket, host = Host, port = Port } = State) ->
    lager:info("~p Disconnected from ~p:~p .\n", [self(), Host, Port]),
    { stop, normal, State };
handle_info({ tcp_error, Socket, _Reason }, _StateName, #state{ socket = Socket, host = Host, port = Port } = State) ->
    lager:info("~p Disconnected due to socket error from ~p:~p .\n", [self(), Host, Port]),
    { stop, normal, State };
handle_info({ { connected, Receiver, ResponseToken }, { Capabilities, ServerID } }, _StateName, #state{ passthrough = Passthrough, passthrough_recv = PassthroughReceiver, tls = TLS } = State) ->
    case TLS of
        starttls ->
            % we do not pass through or notifty when we are going to automatically do a starttls
            % this allows us to send the post-starttls capabilities triggering client activity
            % only AFTER we have completely set up the connectiona, as that usually tends to
            % alter the capabilities
            %
            % if the user of eimap does not want this behavior, they can starttls themselves
            % explicitly
            ok;
        _ ->
            %lager:debug("Connected, capabilities are: ~s; ServerID is ~s", [Capabilities, ServerID]),
            send_hello_string(Capabilities, ServerID, Receiver, ResponseToken, Passthrough, PassthroughReceiver)
    end,
    { next_state, idle, State#state{ parse_state = none, server_id = ServerID } };
handle_info({ { posttls_capabilities, Receiver, ResponseToken }, Capabilities }, _StateName, #state{ server_id = ServerID, passthrough = Passthrough, passthrough_recv = PassthroughReceiver } = State) ->
    OurCapabilities =
        case binary:match(Capabilities, <<"STARTTLS">>) of
            nomatch -> <<Capabilities/binary, " STARTTLS">>;
            _ -> Capabilities
        end,
    send_hello_string(OurCapabilities, ServerID, Receiver, ResponseToken, Passthrough, PassthroughReceiver),
    { next_state, idle, State#state{ parse_state = none } };
handle_info({ { selected, MBox }, ok }, StateName, State) ->
    %%lager:info("~p Selected mbox ~p", [self(), MBox]),
    { next_state, StateName, State#state{ current_mbox = MBox } };
handle_info({ { selected, MBox }, { error, Reason } }, StateName, State) ->
    lager:info("Failed to select mbox ~p: ~p", [MBox, Reason]),
    NewQueue = queue:filter(fun(Command) -> notify_of_mbox_failure_during_filter(Command, Command#command.mbox =:= MBox) end, State#state.command_queue),
    { next_state, StateName, State#state{ command_queue = NewQueue } };
handle_info(starttls_complete, StateName, State) ->
    %lager:info("STARTTLS completed successfully"),
    { next_state, StateName, State };
handle_info(Info, StateName, State) ->
    lager:debug("handle_info called with unhandled info of ~p", [Info]),
    { next_state, StateName, State }.

terminate(_Reason, _Statename, State) -> close_socket(State), ok.

code_change(_OldVsn, Statename, State, _Extra) -> { ok, Statename, State }.

%% private API
send_hello_string(Capabilities, ServerId, Receiver, ResponseToken, Passthrough, PassthroughReceiver) ->
    notify_of_response([{ capabilities, Capabilities }, { server_id, ServerId } ], Receiver, ResponseToken),
    passthrough_capabilities(Capabilities, ServerId, Passthrough, PassthroughReceiver).

passthrough_capabilities(Capabilities, ServerId, true, Receiver) ->
    Message = <<"* OK [CAPABILITY ", Capabilities/binary, "] ", ServerId/binary, "\r\n">>,
    Receiver ! { imap_server_response, Message };
passthrough_capabilities(_Capabilities, _ServerId, _Passthrough, _Receiver) -> ok.

notify_of_response(none, _Command) -> ok;
notify_of_response(Response, #command { from = From, response_token = Token }) -> notify_of_response(Response, From, Token);
notify_of_response(_, _) -> ok.

notify_of_response(_Response, undefined, _Token) -> ok;
notify_of_response(Response, From, undefined) -> From ! Response;
notify_of_response(Response, From, Token) -> From ! { Token, Response }.

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
    %lager:info("Notifying with ~p", [State#state.current_command]),
    notify_of_response(Response, State#state.current_command),
    gen_fsm:send_event(self(), process_command_queue),
    { next_state, idle, State#state{ parse_state = none } };
next_command_after_response(starttls, State) ->
    { TLSState, Socket } = upgrade_socket(State),
    %lager:info("~p Upgraded the socket ...", [self()]),
    gen_fsm:send_event(self(), process_command_queue),
    { next_state, idle, State#state{ parse_state = none, socket = Socket, tls_state = TLSState } };
next_command_after_response(compression_active, State) ->
    { Inflator, Deflator } = eimap_utils:new_imap_compressors(),
    gen_fsm:send_event(self(), process_command_queue),
    { next_state, idle, State#state{ inflator = Inflator, deflator = Deflator } };
next_command_after_response({ close_socket, Response }, State) ->
    notify_of_response(Response, State#state.current_command),
    { stop, normal, State }.

tag_field_width(Serial) when Serial < 10000 -> 4;
tag_field_width(Serial) -> tag_field_width(Serial / 10000, 5).
tag_field_width(Serial, Count) when Serial < 10 -> Count;
tag_field_width(Serial, Count) -> tag_field_width(Serial / 10, Count + 1).

create_socket(Host, Port, true, _Receiver, _ResponseToken, CommandQueue) ->
    { ssl:connect(Host, Port, socket_options(), ?SSL_UPGRADE_TIMEOUT), true, self(), CommandQueue };
create_socket(Host, Port, starttls, Receiver, ResponseToken, CommandQueue) ->
    %lager:debug("Setting up the tls creation with ultimate end point of ~p ~p", [Receiver, ResponseToken]),
    % we do an implicit TLS by adding a starttls command and then a capability command so we can
    % pretend to the user that the socket just magically opened up like this.
    TlsCommand = #command{ message = eimap_command_starttls:new(noparams),
                           from = self(), response_token = undefined,
                           parse_fun = fun eimap_command_starttls:parse/2 },
    CapabilitiesCommand = #command{ message = eimap_command_capability:new(noparams),
                                    from = self(), response_token = { posttls_capabilities, Receiver, ResponseToken },
                                    parse_fun = fun eimap_command_capability:parse/2 },
    % note the use of queue:in_r to _prepend_ the commands so they get run first even if the user
    % has pre-connection queued up commands
    NewCommandQueue = queue:in_r(TlsCommand, queue:in_r(CapabilitiesCommand, CommandQueue)),
    { gen_tcp:connect(Host, Port, socket_options(), ?TCP_CONNECT_TIMEOUT), false, self(), NewCommandQueue };
create_socket(Host, Port, _, _Receiver, _ResponseToken, CommandQueue) ->
    { gen_tcp:connect(Host, Port, socket_options(), ?TCP_CONNECT_TIMEOUT), false, self(), CommandQueue }.

socket_options() -> [binary, { active, once }, { send_timeout, 5000 }].

upgrade_socket(#state{ socket = Socket, tls_state = true, current_command = Command }) ->
    notify_of_response(starttls_complete, Command),
    ssl:setopts(Socket, [{ active, once }]),
    { true, Socket };
upgrade_socket(#state{ socket = Socket, current_command = Command }) ->
    %lager:debug("~p upgrading the server socket due to starttls"[self()]),
    case ssl:connect(Socket, socket_options(), ?SSL_UPGRADE_TIMEOUT) of
        { ok, SSLSocket } ->
            %lager:info("~p it worked", [self()]),
            notify_of_response(starttls_complete, Command),
            ssl:setopts(SSLSocket, [{ active, once }]),
            { true, SSLSocket };
        { error, Reason } ->
            lager:warning("~p StartTLS failed due to: ~p", [self(), Reason]),
            notify_of_response(starttls_failed, Command),
            inet:setopts(Socket, [{ active, once }]),
            { false, Socket }
    end.

close_socket(#state{ socket = undefined }) -> false;
close_socket(#state{ socket = Socket, tls_state = true }) -> ssl:close(Socket);
close_socket(#state{ socket = Socket }) -> gen_tcp:close(Socket).

reset_state(State) -> State#state{ socket = undefined, command_serial = 1 }.

%% sending command code paths:
%%  0. not connected, TLS/SSL, unencrypted
%%  1. no mbox needed, mbox is already selected, mbox needs selecting
send_command(Command, #state{ socket = undefined } = State) ->
    lager:warning("Not connected, dropping command on floor: ~s", [Command]),
    State;
send_command(Command, #state{ tls_state = true} = State) ->
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
    Data = <<Tag/binary, " ", Message/binary, "\r\n">>,
    %lager:info("Sending command via ~p: ~s", [Fun, Data]),
    Fun(Socket, deflated(Data, State)),
    State#state{ command_serial = Serial + 1, current_command = Command#command{ tag = Tag } }.

enque_command(Command, State) ->
    %%lager:info("Enqueuing command ~p", [Command]),
    State#state { command_queue = queue:in(Command, State#state.command_queue) }.

reenque_command(Command, State) ->
    %%lager:info("Re-queueing command ~p", [Command]),
    State#state { command_queue = queue:in_r(Command, State#state.command_queue) }.

inflated(Data, #state{ inflator = undefined }) -> Data;
inflated(Data, #state{ inflator = Inflator }) ->  joined(zlib:inflate(Inflator, Data), <<>>).

deflated(Data, #state{ deflator = undefined }) -> Data;
deflated(Data, #state{ deflator = Deflator }) ->  joined(zlib:deflate(Deflator, Data, sync), <<>>).

joined([], Binary) -> Binary;
joined([H|Rest], Binary) -> joined(Rest, <<Binary/binary, H/binary>>).


