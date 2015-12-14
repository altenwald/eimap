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

-module(eimap_command_examine).
-behavior(eimap_command).
-export([new_command/1, process_line/2, formulate_response/2]).

%% https://tools.ietf.org/html/rfc3501#section-6.3.2

%% Public API
new_command(MBox) when is_list(MBox) -> new_command(list_to_binary(MBox));
new_command(MBox) when is_binary(MBox) -> { <<"EXAMINE \"", MBox/binary, "\"">>, multiline_response }.

%TODO: parse:
% REQUIRED untagged responses: FLAGS, EXISTS, RECENT
% REQUIRED OK untagged responses:  UNSEEN,  PERMANENTFLAGS, UIDNEXT, UIDVALIDITY
process_line(_Data, Acc) -> Acc.

formulate_response(ok, _Acc) -> { fini, ok };
formulate_response({ _, Reason }, _Acc) -> { error, Reason }.
