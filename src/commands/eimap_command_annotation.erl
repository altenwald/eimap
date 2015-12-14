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
-export([new_command/1, process_line/2, formulate_response/2]).

%% Public API
new_command(Folder) when is_list(Folder) -> new_command(list_to_binary(Folder));
new_command(Folder) -> { <<"GETANNOTATION \"", Folder/binary, "\" \"*\" \"value.shared\"">>, multiline_response }.

process_line(<<" ", Data/binary>>, Acc) ->
    process_line(Data, Acc);
process_line(<<"* ANNOTATION ", Data/binary>>, Acc) ->
    Pieces = binary:split(Data, <<"\"">>, [global]),
    process_pieces(Pieces, Acc);
process_line(<<>>, Acc) ->
    Acc.

formulate_response(Result, Data) -> eimap_command:formulate_response(Result, Data).

%% Private API
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

