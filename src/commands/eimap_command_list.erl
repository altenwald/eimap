%% Copyright 2014 Kolab Systems AG (http://www.kolabsys.com)
%%
%% Aaron Seigo (Kolab Systems) <seigo a kolabsys.com>
%%
%% This program is free software: you can redistribute it and/or modify
%% it under the terms of the GNU Library General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU Library General Public License for more details.
%%
%% You should have received a copy of the GNU Library General Public License
%% along with this program.  If not, see <http://www.gnu.org/licenses/>.

-module(eimap_command_list).
-behavior(eimap_command).
-export([new_command/1, process_line/2, formulate_response/2]).

%% Public API
new_command({ Folder }) ->
    Command = metadata_command(Folder),
    { Command, multiline_response }.

process_line(<<"* LIST ", Details/binary>>, Acc) ->
    {match, [_,Attrs,Folder]} = re:run(Details, "\\(([^)]*)\\) \"[^\"]*\" (\"[^\"]+\")", [{capture, all, binary}]),
    [{Attrs, parse_folder(Folder)}|Acc];
process_line(_Line, Acc) -> Acc.

formulate_response(Result, Data) -> eimap_command:formulate_response(Result, Data).

%% Private API
metadata_command(Folder) -> <<"LIST \"\"  \"", Folder/binary, "\"">>.

parse_folder(<<"\"", Rest/binary>>) ->
    { Folder, _RemainingBuffer } = until_closing_quote(Rest),
    { Folder }.

until_closing_quote(Buffer) -> until_closing_quote(Buffer, 0, binary:at(Buffer, 0), 0, <<>>).

until_closing_quote(Buffer, Start, $\\, Pos, Acc) ->
    Escaped = binary:at(Buffer, Pos + 1 ),
    until_closing_quote(Buffer, Start, binary:at(Buffer, Pos + 2), Pos + 2, <<Acc/binary, Escaped>>);
until_closing_quote(Buffer, Start, $", Pos, Acc) ->
    { Acc, binary:part(Buffer, Start + Pos + 1, size(Buffer) - Start - Pos - 1) };
until_closing_quote(Buffer, _Start, Char, Pos, Acc) when Pos =:= size(Buffer) - 1 ->
    { <<Acc/binary, Char>>, <<>> };
until_closing_quote(Buffer, Start, Char, Pos, Acc) ->
    until_closing_quote(Buffer, Start, binary:at(Buffer, Pos + 1), Pos + 1, <<Acc/binary, Char>>).
