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

-module(eimap_command_logout_tests).
-include_lib("eunit/include/eunit.hrl").

parse_test_() ->
    Data =
    [
        % { Binary Response, Binary Tag, Parsed Results }
        {
          <<"abcd OK Begin TLS negotiation now\r\n">>,
          <<"abcd">>,
          { close_socket, ok }
        },
        {
          <<"abcd BAD Uh uh uh\r\n">>,
          <<"abcd">>,
          { close_socket, { error, <<"Uh uh uh">> } }
        },
        {
          <<"abcd NO Uh uh uh\r\n">>,
          <<"abcd">>,
          { close_socket, { error, <<"Uh uh uh">> } }
        }
    ],
    lists:foldl(fun({ Response, Tag, Parsed }, Acc) -> [?_assertEqual(Parsed, eimap_command:parse_response(multiline_response, Response, Tag, eimap_command_logout))|Acc] end, [], Data).

new_test_() ->
    Data =
    [
        % input, output
        { <<>>, { <<"LOGOUT">>, multiline_response } },
        { true, { <<"LOGOUT">>, multiline_response } },
        { [], { <<"LOGOUT">>, multiline_response } }
    ],
    lists:foldl(fun({ Params, Command }, Acc) -> [?_assertEqual(Command, eimap_command_logout:new_command(Params))|Acc] end, [], Data).

