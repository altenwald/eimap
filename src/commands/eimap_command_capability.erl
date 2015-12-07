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
-export([new/1, parse/2]).

%% http://tools.ietf.org/html/rfc2342

%% Public API
new(_Args) -> <<"CAPABILITY">>.

parse(Data, Tag) -> formulate_reponse(eimap_utils:check_response_for_failure(Data, Tag),
                                      eimap_utils:remove_tag_from_response(Data, Tag, check)).


%% Private API
% TODO: probably way too cyrus imap specific on the responses (capitalization, etc)
% make generic with a nicer parser
formulate_reponse(ok, <<"* OK [CAPABILITY ", Data/binary>>) ->
    % this is a server response on connect
    { End, _ } = binary:match(Data, <<"]">>),
    Capabilities = binary:part(Data, { 0, End }),
    { fini, Capabilities };
formulate_reponse(ok, <<"* CAPABILITY ", Data/binary>>) ->
    { End, _ } = binary:match(Data, <<"\r\n">>),
    Capabilities = binary:part(Data, { 0, End }),
    { fini, Capabilities };
formulate_reponse(ok, Data) -> Data;
formulate_reponse({ _, Reason }, _Data) -> { error, Reason }.

