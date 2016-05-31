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

-module(eimap_command_noop_tests).
-include_lib("eunit/include/eunit.hrl").

parse_test_() ->
    Data =
    [
        % { Binary Response, Binary Tag, Parsed Results }
        {
          ok,
          <<"* 100 EXISTS\r\n* 22 EXPUNGE\r\n* 14 FETCH (FLAGS (\\\\Answered \\\\Flagged \\\\Draft \\\\Deleted \\\\Seen))\r\n* 3 Recent\r\nabcd OK Completed\r\n">>,
          <<"abcd">>,
          { fini,
            [
             { recent, 3 },
             { fetch, 14 },
             { expunge, 22 },
             { exists, 100 }
            ]
          }
        },
        {
          ok,
          <<"abcd OK Completed\r\n">>,
          <<"abcd">>,
          { fini, [] }
        },
        {
          ok,
          <<"abcd BAD Uh uh uh\r\n">>,
          <<"abcd">>,
          { error, <<"Uh uh uh">> }
        },
        {
          ok,
          <<"abcd NO Uh uh uh\r\n">>,
          <<"abcd">>,
          { error, <<"Uh uh uh">> }
        }
    ],
    lists:foldl(fun({ InitArgs, ServerResponse, Tag, Parsed }, Acc) ->
                        { _Command, ResponseType } = eimap_command_noop:new_command(InitArgs),
                        [?_assertEqual(Parsed, eimap_command:parse_response(ResponseType, ServerResponse, Tag, eimap_command_noop))|Acc] end, [], Data).

new_test_() ->
    Data =
    [
        % input, output
        { ok, { <<"NOOP">>, multiline_response } },
        { <<>>, { <<"NOOP">>, multiline_response } }
    ],
    lists:foldl(fun({ Params, Command }, Acc) -> [?_assertEqual(Command, eimap_command_noop:new_command(Params))|Acc] end, [], Data).

