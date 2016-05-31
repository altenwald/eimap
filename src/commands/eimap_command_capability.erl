%% Copyright 2015 Kolab Systems AG (http://www.kolabsys.com)
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

-module(eimap_command_capability).
-behavior(eimap_command).
-export([new_command/1, process_line/2, formulate_response/2]).

%% http://tools.ietf.org/html/rfc2342

%% Public API
new_command(parse_serverid) -> { <<"CAPABILITY">>, single_line_response };
new_command(_Args) -> { <<"CAPABILITY">>, multiline_response }.

process_line(<<"* CAPABILITY ", Data/binary>>, Acc) ->
    [Data|Acc];
process_line(_Data, Acc) -> Acc.

formulate_response(ok, Response) -> { fini, Response };
formulate_response({ _, Reason }, _Data) -> { error, Reason };
formulate_response(Data, Tag) -> parse_oneliner(eimap_utils:check_response_for_failure(Data, Tag),
                                                eimap_utils:remove_tag_from_response(Data, Tag, check)).

%% Private API
% TODO: probably way too cyrus imap specific on the responses (capitalization, etc)
% make generic with a nicer parser
parse_oneliner(ok, <<"* OK [CAPABILITY ", Data/binary>>) ->
    % this is a server response on connect
    { End, _ } = binary:match(Data, <<"]">>),
    { TextEnd, _ } = binary:match(Data, <<"\r\n">>),
    Capabilities = binary:part(Data, { 0, End }),
    ServerID = binary:part(Data, { End + 2, TextEnd - End  - 2}),
    { fini, { Capabilities, ServerID } };
parse_oneliner({ _, Reason }, _Data) -> { error, Reason };
parse_oneliner(_, Data) -> { fini, Data }.


