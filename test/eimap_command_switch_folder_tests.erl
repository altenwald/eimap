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

-module(eimap_command_switch_folder_tests).
-include_lib("eunit/include/eunit.hrl").

parse_test_() ->
    Data =
    [
        % { Binary Response, Binary Tag, Parsed Results }
        {
          <<"Trash">>,
          <<"* 0 EXISTS\r\n* 0 RECENT\r\n* FLAGS (\\\\Answered \\\\Flagged \\\\Draft \\\\Deleted \\\\Seen)\r\n* OK [PERMANENTFLAGS ()] Ok\r\n* OK [UIDVALIDITY 1447439786] Ok\r\n* OK [UIDNEXT 1] Ok\r\n* OK [HIGHESTMODSEQ 1] Ok\r\n* OK [URLMECH INTERNAL] Ok>\r\n* OK [ANNOTATIONS 65536] Ok\r\nabcd OK [READ-WRITE] SELECT completed\r\n">>,
          <<"abcd">>,
          { fini,
            [
             { writeable, true },
             { annotations, 65536 },
             { url_mech, internal },
             { highest_mod_seq, 1 },
             { uid_next, 1 },
             { uid_validity, 1447439786 },
             { permanent_flags, [] },
             { flags, [<<"\\\\Answered">>, <<"\\\\Flagged">>, <<"\\\\Draft">>, <<"\\\\Deleted">>, <<"\\\\Seen">>] },
             { recent, 0 },
             { exists, 0 }
            ]
          }
        },
        {
          <<"Nope">>,
          <<"abcd BAD Uh uh uh\r\n">>,
          <<"abcd">>,
          { error, <<"Uh uh uh">> }
        },
        {
          <<"ALsoWrong">>,
          <<"abcd NO Uh uh uh\r\n">>,
          <<"abcd">>,
          { error, <<"Uh uh uh">> }
        }
    ],
    lists:foldl(fun({ InitArgs, ServerResponse, Tag, Parsed }, Acc) ->
                        { _Command, ResponseType } = eimap_command_switch_folder:new_command(InitArgs),
                        [?_assertEqual(Parsed, eimap_command:parse_response(ResponseType, ServerResponse, Tag, eimap_command_switch_folder))|Acc] end, [], Data).

new_test_() ->
    Data =
    [
        % input, output
        { "Trash", { <<"SELECT \"Trash\"">>, all_multiline_response } },
        { <<"Trash">>, { <<"SELECT \"Trash\"">>, all_multiline_response } },
        { { <<"Trash">>, select }, { <<"SELECT \"Trash\"">>, all_multiline_response } },
        { { <<"Trash">>, examine }, { <<"EXAMINE \"Trash\"">>, all_multiline_response } },
        { { "Trash", examine }, { <<"EXAMINE \"Trash\"">>, all_multiline_response } }
    ],
    lists:foldl(fun({ Params, Command }, Acc) -> [?_assertEqual(Command, eimap_command_switch_folder:new_command(Params))|Acc] end, [], Data).

