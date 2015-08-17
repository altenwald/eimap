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

-module(eimap_command_annotation).
-behavior(eimap_command).
-export([new/1, parse/2]).

%% Public API
new(Folder) when is_binary(Folder) ->
    <<"GETANNOTATION \"", Folder/binary, "\" \"*\" \"value.shared\"">>.

parse(Data, Tag) when is_binary(Data) ->
    case eimap_utils:check_response_for_failure(Data, Tag) of
        ok ->
            Lines = binary:split(Data, <<"\r\n">>, [global]),
            Rv = lists:foldl(fun(Line, Acc) -> parseLine(Line, Acc, Tag) end, [], Lines),
            { fini, Rv };
        { _, Reason } ->
            { error, Reason }
    end.


%% Private API
parseLine(<<" ", Data/binary>>, Acc, Tag) ->
    parseLine(Data, Acc, Tag);
parseLine(<<"* ANNOTATION ", Data/binary>>, Acc, _Tag) ->
    Pieces = binary:split(Data, <<"\"">>, [global]),
    process_pieces(Pieces, Acc);
parseLine(<<>>, Acc, _Tag) ->
    Acc;
parseLine(Data, Acc, Tag) ->
    case binary:match(Data, Tag) of
        { 0, End } -> handle_possible_end(binary:part(Data, End + 1, byte_size(Data) - End - 1), Acc);
        _ -> lager:warning("Unexpected response from imap server: ~p", [Data]), Acc
    end.

process_pieces([MBox, Key, _, _, _, Value, _], Acc) when MBox =/= <<>> -> [ { Key, translate(Value) } | Acc ];
process_pieces([_, _MBox, _, Key, _, _, _, Value, _], Acc) -> [ { Key, translate(Value) } | Acc ];
process_pieces(_, Acc) -> Acc.

translate(<<"false">>) -> false;
translate(<<"true">>) -> true;
translate(Value) ->
    try list_to_integer(binary_to_list(Value)) of
        Number -> Number
    catch
        _:_ -> Value
    end.

handle_possible_end(<<"OK", _/binary>>, Acc) ->
    Acc;
handle_possible_end(<<"NO ", Reason/binary>>, Acc) ->
    lager:warning("Annotation error from imap server: ~p", [Reason]),
    Acc;
handle_possible_end(<<"BAD ", Reason/binary>>, Acc) ->
    lager:warning("Annotation error from imap server: ~p", [Reason]),
    Acc;
handle_possible_end([_Tag, Message], Acc) ->
    handle_possible_end(Message, Acc);
handle_possible_end(Message, Acc) ->
    lager:warning("Unexpected response from imap server: ~p", [Message]),
    Acc.
