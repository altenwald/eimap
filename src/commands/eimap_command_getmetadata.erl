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
-export([new/1, parse/2, continue_parse/3]).

%% http://tools.ietf.org/html/rfc2342

%% Public API
new({ Folder }) -> new({ Folder, [] });
new({ Folder, Attributes }) when is_list(Folder) -> new({ list_to_binary(Folder), Attributes });
new({ Folder, Attributes }) ->
    AttributesList = format_attributes(Attributes, <<>>),
    Command = <<"GETMETADATA (DEPTH infinity) \"", Folder/binary, "\"", AttributesList/binary>>,
    io:format("Sending ~p~n", [Command]),
    Command.

parse(Data, Tag) -> continue_parse(Data, Tag, { <<>>, [] }).

continue_parse(Data, Tag, { LastPartialLine, Acc }) ->
    FullBuffer = <<LastPartialLine/binary, Data/binary>>,
    { FullLinesBuffer, LastPartialLine } = eimap_utils:only_full_lines(FullBuffer),
    io:format("Metadata response parsing: ~p~n~n", [Data]),
    Lines = binary:split(FullLinesBuffer, <<"\r\n">>, [global]),
    process_line(Tag, LastPartialLine, Lines, Acc).


%% Private API
process_line(_Tag, LastPartialLine, [], Acc) -> { more, fun ?MODULE:continue_parse/3, { LastPartialLine, Acc } };
process_line(Tag, LastPartialLine, [Line|MoreLines], Acc) ->
    case eimap_utils:is_tagged_response(Line, Tag) of
        true ->
            formulate_response(eimap_utils:check_response_for_failure(Line, Tag), Acc);
        false ->
            process_line(Tag, LastPartialLine, MoreLines, process_metadata_response(Line, Acc))
    end.

formulate_response(ok, Data) -> { fini, Data };
formulate_response({ _, Reason }, _Data) -> { error, Reason }.

process_metadata_response(Line, Acc) -> io:format("Metadata is back: ~p~n", [Line]), Acc.

format_attributes([], <<>>) -> <<>>;
format_attributes([], String) -> <<" (", String/binary, ")">>;
format_attributes([Attribute|Attributes], String) ->
    AttrBin = case is_list(Attribute) of
                true -> io:format("Chaning up ... ~p~n", [Attribute]), list_to_binary(Attribute);
                false -> Attribute
              end,
    case String of
        <<>> -> format_attributes(Attributes, AttrBin);
        _ -> format_attributes(Attributes, <<String/binary, " ", AttrBin/binary>>)
    end;
format_attributes(_, _String) -> <<>>.

