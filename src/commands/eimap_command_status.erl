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

-module(eimap_command_status).
-behavior(eimap_command).
-export([new/1, parse/2, continue_parse/3]).

% https://tools.ietf.org/html/rfc5464

% supported attributes: messages, recent, uidnext, uidvalidity, unseen, annotate
% Public API
new({ Folder, Attributes }) when is_list(Folder) -> new({ list_to_binary(Folder), Attributes });
new({ <<>>, Attributes}) -> new({ <<"INBOX">>, Attributes });
new({ Folder, []}) -> new({ Folder, [messages] });
new({ Folder, Attributes }) when is_list(Attributes) ->
          AttributesString = attribute_string(Attributes, <<>>),
          <<"STATUS ", Folder/binary, " (", AttributesString/binary, ")">>.

parse(Data, Tag) -> continue_parse(Data, Tag, []).

continue_parse(Data, Tag, Acc) ->
    Lines = binary:split(Data, <<"\r\n">>, [global]),
    process_line(Tag, Lines, Acc).


%% Private API
attribute_string([], <<>>) -> attribute_string(messages);
attribute_string([], String) -> String;
attribute_string([Attribute|Attributes], <<>>) -> 
    Attr = attribute_string(Attribute),
    attribute_string(Attributes, Attr);
attribute_string([Attribute|Attributes], String) -> 
    case attribute_string(Attribute) of
        <<>> -> attribute_string(Attributes, String);
        Attr -> attribute_string(Attributes, <<String/binary, " ", Attr/binary>>)
    end.

process_line(_Tag, [], Acc) -> { more, fun ?MODULE:continue_parse/3, Acc };
process_line(Tag, [Line|MoreLines], Acc) ->
    case eimap_utils:is_tagged_response(Line, Tag) of
        true ->
            formulate_response(eimap_utils:check_response_for_failure(Line, Tag), Acc);
        false ->
            process_line(Tag, MoreLines, process_status_items(Line, Acc))
    end.

%TODO: multiline, partial lines
formulate_response(ok, Acc) -> { fini, Acc };
formulate_response({ _, Reason }, _Acc) -> { error, Reason }.

process_status_items(<<"* STATUS ", Data/binary>>, Acc) ->
    process_status_items(binary:match(Data, <<"(">>),
                         binary:match(Data, <<")">>),
                         Data, Acc);
process_status_items(_, Acc) -> Acc.

process_status_items(nomatch, _, _Data, Acc) -> Acc;
process_status_items(_, nomatch, _Data, Acc) -> Acc;
process_status_items({ Start, _ }, { End, _ }, Data, Acc) ->
    Parts = binary:split(binary:part(Data, Start + 1, End - Start - 1), <<" ">>, [global]),
    process_next_status_item(Parts, Acc).

process_next_status_item([], Acc) -> Acc;
process_next_status_item([_], Acc) -> Acc;
process_next_status_item([Token, Value|Parts], Acc) ->
    Tuple = { attribute_atom(Token), binary_to_integer(Value) },
    process_next_status_item(Parts, [Tuple|Acc]).

attribute_string(messages) -> <<"MESSAGES">>;
attribute_string(recent) -> <<"RECENT">>;
attribute_string(uidnext) -> <<"UIDNEXT">>;
attribute_string(uidvalidity) -> <<"UIDVALIDITY">>;
attribute_string(unseen) -> <<"UNSEEN">>;
attribute_string(_) -> <<>>.

attribute_atom(<<"MESSAGES">>) -> messages;
attribute_atom(<<"RECENT">>) -> recent;
attribute_atom(<<"UIDNEXT">>) -> uidnext;
attribute_atom(<<"UIDVALIDITY">>) -> uidvalidity;
attribute_atom(<<"UNSEEN">>) -> unseen.

