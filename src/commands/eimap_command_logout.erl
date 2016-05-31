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

-module(eimap_command_logout).
-behavior(eimap_command).
-export([new_command/1, process_line/2, formulate_response/2]).

%% Public API
new_command(_Args) -> { <<"LOGOUT">>, multiline_response }.

process_line(_Data, Acc) -> Acc.

formulate_response(ok, _Data) -> { close_socket, ok };
formulate_response({ _, Reason }, _Data) -> { close_socket, { error, Reason } }.

