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

-module(eimap_command_namespace).
-behavior(eimap_command).
-export([new/1, parse/2, continue_parse/3]).

%% http://tools.ietf.org/html/rfc2342

%% Public API
new(_Args) -> <<"NAMESPACE">>.

%TODO support multi-packet continuing parse
parse(Data, Tag) -> continue_parse(Data, Tag, []).

continue_parse(Data, Tag, Acc) ->
    Lines = binary:split(Data, <<"\r\n">>, [global]), %%TODO partial lines
    process_line(Tag, Lines, Acc).


%% Private API
process_line(_Tag, [], Acc) -> { more, fun ?MODULE:continue_parse/3, Acc };
process_line(Tag, [Line|MoreLines], Acc) ->
    case eimap_utils:is_tagged_response(Line, Tag) of
        true ->
            formulate_response(eimap_utils:check_response_for_failure(Line, Tag), Acc);
        false ->
            process_line(Tag, MoreLines, [process_shared_prefix_parts(Line)|Acc])
    end.

%TODO: multiline!
formulate_response(ok, [Acc]) -> { fini, relevant_shared_prefix_parts(Acc) };
formulate_response({ _, Reason }, _Acc) -> { error, Reason }.

relevant_shared_prefix_parts([]) -> { none, none};
relevant_shared_prefix_parts([[], [], _]) -> { none, none };
relevant_shared_prefix_parts([[], Delim]) -> { none, Delim };
relevant_shared_prefix_parts([SharedPrefix, [], _]) -> { SharedPrefix, "/" };
relevant_shared_prefix_parts([SharedPrefix, Delim]) -> { SharedPrefix, Delim }.

process_shared_prefix_parts(<<"* NAMESPACE ", Data/binary>>) -> process_shared_prefix_parts(Data, 1);
process_shared_prefix_parts(_) -> [].

process_shared_prefix_parts(<<"NIL ", Data/binary>>, PartNumber) ->
    process_shared_prefix_parts(Data, PartNumber + 1);
process_shared_prefix_parts(<<"NIL", Data/binary>>, PartNumber) ->
    process_shared_prefix_parts(Data, PartNumber + 1);
process_shared_prefix_parts(<<"((", Data/binary>>, PartNumber) ->
    process_shared_prefix_parts_inner(Data, [], PartNumber);
process_shared_prefix_parts(<<>>, _PartNumber) -> [].

process_shared_prefix_parts_inner(<<"))", _/binary>>, Acc, 3) ->
    parse_shared_prefix_fields(lists:reverse(Acc), []);
process_shared_prefix_parts_inner(<<")) ", Data/binary>>, _Acc, N) ->
    process_shared_prefix_parts(Data, N + 1);
process_shared_prefix_parts_inner(<<Char, Data/binary>>, Acc, 3) ->
    process_shared_prefix_parts_inner(Data, [Char|Acc], 3);
process_shared_prefix_parts_inner(<<_Char, Data/binary>>, Acc, N) ->
    process_shared_prefix_parts_inner(Data, Acc, N);
process_shared_prefix_parts_inner(<<>>, _Acc, _N) ->
    [].

%%FIXME: naively assumes no "s in the values are allowed. is this correct?
parse_shared_prefix_fields([$"|Tail], Acc) ->
    parse_shared_prefix_field_inner(Tail, [], Acc);
parse_shared_prefix_fields([_|Tail], Acc) ->
    parse_shared_prefix_fields(Tail, Acc);
parse_shared_prefix_fields([], Acc) ->
    lists:reverse(Acc).

parse_shared_prefix_field_inner([$"|Tail], PartAcc, Acc) ->
    parse_shared_prefix_fields(Tail, [lists:reverse(PartAcc)|Acc]);
parse_shared_prefix_field_inner([Char|Tail], PartAcc, Acc) ->
    parse_shared_prefix_field_inner(Tail, [Char|PartAcc], Acc);
parse_shared_prefix_field_inner([], _PartAcc, Acc) ->
    Acc.

