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

-module(eimap_command_tests).
-include_lib("eunit/include/eunit.hrl").
-export([process_line/2, formulate_response/2]).

% c("test/eimap_uidset_tests.erl"). eunit:test(eimap_uidset).

process_line(Data, Acc) -> [Data | Acc].
formulate_response(Result, Data) -> eimap_command:formulate_response(Result, lists:reverse(Data)).


literal_continuations_test_() ->
    Data =
    [
        % input, output
        % { Binary Response, Binary Tag, Parsed Results }
        {
          <<"* STATUS blurdybloop (MESSAGES 231 {14+}\r\nUIDNEXT 44292)\r\nabcd OK Begin TLS negotiation now\r\n">>,
          <<"abcd">>,
          { fini, [<<"* STATUS blurdybloop (MESSAGES 231 UIDNEXT 44292)">>] }
        },
        {
          <<"* STATUS blurdybloop (MESSAGES 231 {h14+}\r\nUIDNEXT 44292)\r\nabcd OK Begin TLS negotiation now\r\n">>,
          <<"abcd">>,
          { fini, [<<"* STATUS blurdybloop (MESSAGES 231 {h14+}">>, <<"UIDNEXT 44292)">>] }
        },
        {
          <<"* STATUS blurdybloop (MESSAGES 231 \r\nUIDNEXT 44292)\r\nabcd OK Begin TLS negotiation now\r\n">>,
          <<"abcd">>,
          { fini, [<<"* STATUS blurdybloop (MESSAGES 231 ">>, <<"UIDNEXT 44292)">>] }
        },
        {
          <<"* STATUS blurdybloop (MESSAGES 231 UIDNEXT 44292)\r\nabcd OK Begin TLS negotiation now\r\n">>,
          <<"abcd">>,
          { fini, [<<"* STATUS blurdybloop (MESSAGES 231 UIDNEXT 44292)">>] }
        }
    ],
    lists:foldl(fun({ Binary, Tag, Result }, Acc) -> [?_assertEqual(Result, eimap_command:parse_response(multiline_response, Binary, Tag, eimap_command_tests))|Acc] end, [], Data).

