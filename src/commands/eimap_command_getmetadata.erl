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

-module(eimap_command_getmetadata).
-behavior(eimap_command).
-export([new_command/1, process_line/2, formulate_response/2]).

%% http://tools.ietf.org/html/rfc2342

%% Public API
new_command({ Folder }) -> new_command({ Folder, [] });
new_command({ Folder, Attributes }) -> new_command({ Folder, Attributes, infinity, nomax });

%% Depth and MaxSize are both optional arguments, so we have a 4-tuple version of new_command to accomodate this
%% "Depth, Maxsize" could be replaced with a proplist of the form [ { depth, Depth }, { maxsize, MaxSize } ] but
%% as this is an internal (to eimap) API there is little benefit for the performance cost
new_command({ Folder, Attributes, Depth, MaxSize }) when is_list(Folder) -> new_command({ list_to_binary(Folder), Attributes, Depth, MaxSize });
new_command({ Folder, Attributes, Depth, MaxSize }) ->
    AttributesString = format_attributes(Attributes, <<>>),
    DepthString = depth_param(Depth),
    MaxSizeString = maxsize_param(MaxSize),
    Command = metadata_command(DepthString, MaxSizeString, Folder, AttributesString),
    { Command, multiline_response }.

process_line(<<"* METADATA ", Details/binary>>, Acc) ->
    Results = parse_folder(Details),
    [Results|Acc];
process_line(_Line, Acc) -> Acc.

formulate_response(Result, Data) -> eimap_command:formulate_response(Result, Data).

%% Private API
depth_param(infinity) -> <<"DEPTH infinity">>;
depth_param(Depth) when is_integer(Depth) -> Bin = integer_to_binary(Depth), <<"DEPTH ", Bin/binary>>;
depth_param(_) -> <<>>.

maxsize_param(Size) when is_integer(Size) -> Bin = integer_to_binary(Size), <<"MAXSIZE ", Bin/binary>>;
maxsize_param(_) -> <<>>.

metadata_command(<<>>, <<>>, Folder, Attributes) -> <<"GETMETADATA \"", Folder/binary, "\"", Attributes/binary>>;
metadata_command(Depth, <<>>, Folder, Attributes) -> <<"GETMETADATA (", Depth/binary, ") \"", Folder/binary, "\"", Attributes/binary>>;
metadata_command(<<>>, MaxSize, Folder, Attributes) -> <<"GETMETADATA (", MaxSize/binary, ") \"", Folder/binary, "\"", Attributes/binary>>;
metadata_command(Depth, MaxSize, Folder, Attributes) -> <<"GETMETADATA (", Depth/binary, " ", MaxSize/binary, ") \"", Folder/binary, "\"", Attributes/binary>>.

format_attributes([], <<>>) -> <<>>;
format_attributes([], String) -> <<" (", String/binary, ")">>;
format_attributes([Attribute|Attributes], String) ->
    AttrBin = case is_list(Attribute) of
                true -> list_to_binary(Attribute);
                false -> Attribute
              end,
    case String of
        <<>> -> format_attributes(Attributes, AttrBin);
        _ -> format_attributes(Attributes, <<String/binary, " ", AttrBin/binary>>)
    end;
format_attributes(_, _String) -> <<>>.

parse_folder(<<"\"", Rest/binary>>) ->
    { Folder, RemainingBuffer } = until_closing_quote(Rest),
    Properties = parse_properties(RemainingBuffer),
    { Folder, Properties };
parse_folder(Buffer) ->
    { FolderEnd, _ } = binary:match(Buffer, <<" ">>),
    Folder = binary:part(Buffer, 0, FolderEnd),
    Properties = parse_properties(binary:part(Buffer, FolderEnd, size(Buffer) - FolderEnd)),
    { Folder, Properties }.

parse_properties(Buffer) ->
    { Start, _ } = binary:match(Buffer, <<"(">>),
    { End, _ } = binary:match(Buffer, <<")">>),
    Properties = binary:part(Buffer, Start + 1, End - Start - 1),
    next_property(Properties, []).

next_property(<<>>, Acc) -> Acc;
next_property(Buffer, Acc) ->
    { KeyEnd, _ } = binary:match(Buffer, <<" ">>),
    Key = binary:part(Buffer, 0, KeyEnd),
    { Value, RemainingBuffer } = 
        case next_value(binary:part(Buffer, KeyEnd + 1, size(Buffer) - KeyEnd - 1)) of
            { <<"NIL">>, RBuffer } -> { null, RBuffer };
            Rv -> Rv
        end,
    next_property(RemainingBuffer, [{ Key, Value }|Acc]).

next_value(<<"\"", Rest/binary>>) ->
    until_closing_quote(Rest);
next_value(Buffer) ->
    case binary:match(Buffer, <<" ">>) of
        nomatch -> { Buffer, <<>> };
        { ValueEnd , _ } -> { binary:part(Buffer, 0, ValueEnd - 1), binary:part(Buffer, ValueEnd, size(Buffer) - ValueEnd) }
    end.

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
