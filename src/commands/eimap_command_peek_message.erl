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

-module(eimap_command_peek_message).
-behavior(eimap_command).
-export([new_command/1, parse/2, continue_parse/3]).
-record(parse_state, { body_size, parts, data }).
-record(parts, { headers = <<"">>, body = <<"">>, flags = <<"">> }).

%% https://tools.ietf.org/html/rfc3501#section-6.4.5

%% Public API
new_command(MessageID) when is_integer(MessageID) -> new_command(integer_to_binary(MessageID));
new_command(MessageID) when is_binary(MessageID) -> { <<"UID FETCH ",  MessageID/binary, " (FLAGS BODY.PEEK[HEADER] BODY.PEEK[TEXT])">>, blob_response }.

continue_parse(Data, _Tag, #parse_state{ body_size = Size, parts = Parts, data = PrevData }) ->
    try_body_parse(<<PrevData/binary, Data/binary>>, Size, Parts).

parse(Data, Tag) when is_binary(Data) ->
    case eimap_utils:check_response_for_failure(Data, Tag) of
        ok -> process_parts(get_past_headers(Data));
        { _, Reason } -> { error, Reason }
    end.

%% Private API
process_parts(#parts{ headers = Headers, flags = Flags, body = Body }) ->
    { fini, [ { flags, binary:split(Flags, <<" ">>, [global]) },
              { headers, filter_headers(binary:split(Headers, <<"\r\n">>, [global])) },
              { message, <<Headers/binary, Body/binary>> } ] };
process_parts(Result) ->
    Result.

get_past_headers(<<" OK ", _Data/binary>>) -> #parts{};
get_past_headers(<<" FETCH ", Data/binary>>) -> find_open_parens(Data);
get_past_headers(<<_, Data/binary>>) -> get_past_headers(Data);
get_past_headers(<<>>) -> { error, <<"Unparsable">> }.

find_open_parens(<<$(, Data/binary>>) -> parse_next_component(Data, #parts{});
find_open_parens(<<_, Data/binary>>) -> find_open_parens(Data).

parse_next_component(<<"FLAGS (", Data/binary>>, Parts) ->
    parse_flags(Data, Parts);
parse_next_component(<<"BODY[HEADER] {", Data/binary>>, Parts) ->
    parse_header(Data, Parts);
parse_next_component(<<"BODY[TEXT] {", Data/binary>>, Parts) ->
    parse_body(Data, Parts);
parse_next_component(<<_, Data/binary>>, Parts) -> parse_next_component(Data, Parts);
parse_next_component(<<"OK Completed", _/binary>>, Parts) -> Parts;
parse_next_component(<<>>, Parts) -> Parts.

parse_flags(Data, Parts) -> parse_flags(Data, Parts, Data, 0).
parse_flags(OrigData, Parts, <<$), Rest/binary>>, Length) ->
    FlagsString = binary:part(OrigData, 0, Length),
    parse_next_component(Rest, Parts#parts{ flags = FlagsString });
parse_flags(OrigData, Parts, <<_, Data/binary>>, Length) ->
    parse_flags(OrigData, Parts, Data, Length + 1);
parse_flags(_OrigData, Parts, <<>>, _Length) ->
    Parts.

parse_header(Data, Parts) -> parse_header(Data, Parts, Data, 0).
parse_header(_OrigData, Parts, <<$}, Rest/binary>>, 0) ->
    parse_next_component(Rest, Parts);
parse_header(OrigData, Parts, <<$}, Rest/binary>>, Length) ->
    ByteSizeString = binary:part(OrigData, 0, Length),
    Size = binary_to_integer(ByteSizeString),
    HeaderString = binary:part(Rest, 2, Size), %% the 2 is for \r\n
    %%FIXME: make sure we have enough data loaded, otherwise continue
    Remainder = binary:part(Rest, Size, byte_size(Rest) - Size),
    parse_next_component(Remainder, Parts#parts{ headers = HeaderString });
parse_header(OrigData, Parts, <<_, Rest/binary>>, Length) ->
    parse_header(OrigData, Parts, Rest, Length + 1);
parse_header(_OrigData, Parts, <<>>, _Length) ->
    Parts.

parse_body(Data, Parts) -> parse_body(Data, Parts, Data, 0).
parse_body(_OrigData, Parts, <<$}, Rest/binary>>, 0) ->
    parse_next_component(Rest, Parts);
parse_body(OrigData, Parts, <<$}, Rest/binary>>, Length) ->
    ByteSizeString = binary:part(OrigData, 0, Length),
    Size = binary_to_integer(ByteSizeString),
    %%lager:info("We have ... ~p ~p ~p", [ByteSizeString, Size, byte_size(Rest)]),
    try_body_parse(Rest, Size, Parts);
parse_body(OrigData, Parts, <<_, Rest/binary>>, Length) ->
    parse_body(OrigData, Parts, Rest, Length + 1);
parse_body(_OrigData, Parts, <<>>, _Length) ->
    Parts.

try_body_parse(Data, Size, Parts) ->
    case Size > byte_size(Data) of
        true ->
            { more, fun ?MODULE:continue_parse/3, #parse_state{ body_size = Size, parts = Parts, data = Data } };
        false ->
            Body = binary:part(Data, 2, Size), %% the 2 is for \r\n
            Remainder = binary:part(Data, Size, byte_size(Data) - Size),
            process_parts(parse_next_component(Remainder, Parts#parts{ body = Body }))
    end.

filter_headers(RawHeaders) ->
    filter_headers(RawHeaders, none, none, []).

filter_headers([], CurrentKey, CurrentValue, Acc) ->
    filter_header_add(CurrentKey, CurrentValue, Acc);
filter_headers([<<>>|Headers], CurrentKey, CurrentValue, Acc) ->
    filter_headers(Headers, CurrentKey, CurrentValue, Acc);
filter_headers([Header|Headers], CurrentKey, CurrentValue, Acc) ->
    filter_header(Headers, CurrentKey, CurrentValue, Acc, binary:split(Header, <<": ">>)).

filter_header(Headers, CurrentKey, CurrentValue, Acc, [Key, Value]) ->
    Added = filter_header_add(CurrentKey, CurrentValue, Acc),
    filter_headers(Headers, Key, Value, Added);
filter_header(Headers, none, _CurrentValue, Acc, _Value) ->
    filter_headers(Headers, none, none, Acc);
filter_header(Headers, CurrentKey, CurrentValue, Acc, [Value]) ->
    %%TODO: get rid of tab, ensure "proper" whitespace
    NewValue = <<CurrentValue/binary, Value/binary>>,
    filter_headers(Headers, CurrentKey, NewValue, Acc).

filter_header_add(CurrentKey, CurrentValue, Acc) when CurrentKey =/= none, CurrentValue =/= none ->
    %%TODO: split CurrentValue on ','
    [ { CurrentKey, CurrentValue } | Acc ];
filter_header_add(_, _, Acc) ->
    Acc.

