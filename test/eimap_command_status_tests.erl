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

-module(eimap_command_status_tests).
-include_lib("eunit/include/eunit.hrl").

parse_test_() ->
    Data =
    [
        % { Binary Response, Binary Tag, Parsed Results }
        {
          <<"* STATUS blurdybloop (MESSAGES 231 UIDNEXT 44292)\r\nabcd OK Begin TLS negotiation now\r\n">>,
          <<"abcd">>,
          { fini, [{ uidnext, 44292 }, { messages, 231 }] }
        },
        {
          <<"* STATUS blurdybloop (MESSAGES 231 UIDNEXT 44292 RECENT 0 UIDVALIDITY 10110 UNSEEN 10)\r\nabcd OK Begin TLS negotiation now\r\n">>,
          <<"abcd">>,
          { fini, [{ unseen, 10 }, { uidvalidity, 10110 }, { recent, 0 }, { uidnext, 44292 }, { messages, 231 }] }
        },
        {
          <<"* STATUS blurdybloop (RECENT 1 UIDVALIDITY 44292)\r\nabcd OK Begin TLS negotiation now\r\n">>,
          <<"abcd">>,
          { fini, [{ uidvalidity, 44292 }, { recent, 1 }] }
        }
    ],
    lists:foldl(fun({ Response, Tag, Parsed }, Acc) -> [?_assertEqual(Parsed, eimap_command:parse_response(multiline_response, Response, Tag, eimap_command_status))|Acc] end, [], Data).

new_test_() ->
    Data =
    [
        % input, output
        { { "INBOX", [] },
          { <<"STATUS INBOX (MESSAGES)">>, multiline_response } },
        { { <<"INBOX">>, [] },
          { <<"STATUS INBOX (MESSAGES)">>, multiline_response } },
        { { <<>>, [messages] },
          { <<"STATUS INBOX (MESSAGES)">>, multiline_response } },
        { { <<"">>, [messages] },
          { <<"STATUS INBOX (MESSAGES)">>, multiline_response } },
        { { <<"/my/folder">>, [messages, recent, uidnext, uidvalidity, unseen] },
          { <<"STATUS /my/folder (MESSAGES RECENT UIDNEXT UIDVALIDITY UNSEEN)">>, multiline_response } },
        { { <<"/my/folder">>, [uidnext, recent, uidvalidity, unseen] },
          { <<"STATUS /my/folder (UIDNEXT RECENT UIDVALIDITY UNSEEN)">>, multiline_response } },
        { { <<"/my/folder">>, [garbage, unseen] },
          { <<"STATUS /my/folder (UNSEEN)">>, multiline_response } },
        { { <<"/my/folder">>, [garbage] },
          { <<"STATUS /my/folder (MESSAGES)">>, multiline_response } }
    ],
    lists:foldl(fun({ Params, Command }, Acc) -> [?_assertEqual(Command, eimap_command_status:new_command(Params))|Acc] end, [], Data).

