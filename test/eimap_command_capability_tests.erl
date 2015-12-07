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

-module(eimap_command_capability_tests).
-include_lib("eunit/include/eunit.hrl").

parse_test_() ->
    Data =
    [
        % { Binary Response, Binary Tag, Parsed Results }
        {
          <<"* OK [CAPABILITY IMAP4rev1 LITERAL+ ID ENABLE STARTTLS LOGINDISABLED] acme.com Cyrus IMAP 2.5.5.5-Kolab-2.5.5-5.1.el6.kolab_14 server ready\r\n">>,
          <<>>,
          { fini, <<"IMAP4rev1 LITERAL+ ID ENABLE STARTTLS LOGINDISABLED">> }
        },
        {
          <<"abcd CAPABILITY IMAP4rev1 LITERAL+ ID ENABLE STARTTLS AUTH=PLAIN AUTH=LOGIN SASL-IR\r\nabcd OK CAPABILITY COMPLETED\r\n">>,
          <<"abcd">>,
          { fini, <<"IMAP4rev1 LITERAL+ ID ENABLE STARTTLS AUTH=PLAIN AUTH=LOGIN SASL-IR">> }
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
    lists:foldl(fun({ Response, Tag, Parsed }, Acc) -> [?_assertEqual(Parsed, eimap_command_capability:parse(Response, Tag))|Acc] end, [], Data).

new_test_() ->
    Data =
    [
        % input, output
        { <<>>, <<"CAPABILITY">> },
        { true, <<"CAPABILITY">> },
        { [], <<"CAPABILITY">> }
    ],
    lists:foldl(fun({ Params, Command }, Acc) -> [?_assertEqual(Command, eimap_command_capability:new(Params))|Acc] end, [], Data).

