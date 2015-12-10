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

-module(eimap_command_getmetadata_tests).
-include_lib("eunit/include/eunit.hrl").

parse_test_() ->
    Data =
    [
        % { Binary Response, Binary Tag, Parsed Results }
        {
          <<"* METADATA Tasks (/shared/vendor/kolab/folder-type \"task\")\r\nabcd OK Begin TLS negotiation now\r\n">>,
          <<"abcd">>,
          { fini, [ { <<"Tasks">>, [ { <<"/shared/vendor/kolab/folder-type">>, <<"task">> } ] } ] }
        },
        {
          <<"* METADATA Tasks (/shared/vendor/kolab/folder-type \"task \\\"sigh\\\"\")\r\nabcd OK Begin TLS negotiation now\r\n">>,
          <<"abcd">>,
          { fini, [ { <<"Tasks">>, [ { <<"/shared/vendor/kolab/folder-type">>, <<"task \"sigh\"">> } ] } ] }
        },
        {
          <<"* METADATA Tasks (/shared/vendor/kolab/folder-type \"task \\\"sigh\\\"\")\r\n* METADATA Archive (/shared/vendor/kolab/folder-type NIL)\r\nabcd OK Begin TLS negotiation now\r\n">>,
          <<"abcd">>,
          { fini, [
                    { <<"Archive">>, [ {<<"/shared/vendor/kolab/folder-type">>, null } ] },
                    { <<"Tasks">>, [ { <<"/shared/vendor/kolab/folder-type">>, <<"task \"sigh\"">> } ] }
                  ]
          }
        },
        {
          <<"abcd BAD Uh uh uh\r\n">>,
          <<"abcd">>,
          { error, <<"Uh uh uh">> }
        },
        {
          <<"abcd NO Uh uh uh\r\n">>,
          <<"abcd">>,
          { error, <<"Uh uh uh">> }
        }
    ],
    lists:foldl(fun({ Response, Tag, Parsed }, Acc) -> [?_assertEqual(Parsed, eimap_command_getmetadata:parse(Response, Tag))|Acc] end, [], Data).

new_test_() ->
    Data =
    [
        % input, output
        { { <<>> }, <<"GETMETADATA (DEPTH infinity) \"\"">> },
        { { <<>>, [<<"/shared/comment">>, "/private/comment"] }, <<"GETMETADATA (DEPTH infinity) \"\" (/shared/comment /private/comment)">> },
        { { <<"/my/folder">>, [<<"/shared/comment">>, "/private/comment"] }, <<"GETMETADATA (DEPTH infinity) \"/my/folder\" (/shared/comment /private/comment)">> },
        { { "/my/folder", [<<"/shared/comment">>, "/private/comment"] }, <<"GETMETADATA (DEPTH infinity) \"/my/folder\" (/shared/comment /private/comment)">> },
        { { <<"/my/folder">> }, <<"GETMETADATA (DEPTH infinity) \"/my/folder\"">> }
    ],
    lists:foldl(fun({ Params, Command }, Acc) -> [?_assertEqual(Command, eimap_command_getmetadata:new(Params))|Acc] end, [], Data).

