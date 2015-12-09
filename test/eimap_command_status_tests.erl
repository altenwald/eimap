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
        }
    ],
    lists:foldl(fun({ Response, Tag, Parsed }, Acc) -> [?_assertEqual(Parsed, eimap_command_status:parse(Response, Tag))|Acc] end, [], Data).

new_test_() ->
    Data =
    [
        % input, output
        { { "INBOX", [] }, <<"STATUS INBOX (MESSAGES)">> },
        { { <<"INBOX">>, [] }, <<"STATUS INBOX (MESSAGES)">> },
        { { <<>>, [messages] }, <<"STATUS INBOX (MESSAGES)">> },
        { { <<"">>, [messages] }, <<"STATUS INBOX (MESSAGES)">> },
        { { <<"/my/folder">>, [messages, recent, uidnext, uidvalidity, unseen] },
           <<"STATUS /my/folder (MESSAGES RECENT UIDNEXT UIDVALIDITY UNSEEN)">> },
        { { <<"/my/folder">>, [uidnext, recent, uidvalidity, unseen] },
           <<"STATUS /my/folder (UIDNEXT RECENT UIDVALIDITY UNSEEN)">> },
        { { <<"/my/folder">>, [garbage, unseen] },
           <<"STATUS /my/folder (UNSEEN)">> },
        { { <<"/my/folder">>, [garbage] }, <<"STATUS /my/folder (MESSAGES)">> }
    ],
    lists:foldl(fun({ Params, Command }, Acc) -> [?_assertEqual(Command, eimap_command_status:new(Params))|Acc] end, [], Data).

