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

-module(eimap_command_noop).
-behavior(eimap_command).
-export([new_command/1, process_line/2, formulate_response/2]).

%% https://tools.ietf.org/html/rfc3501#section-6.3.2

%% Public API
new_command(_) -> { <<"NOOP">>, multiline_response }.

process_line(Data, Acc) -> eimap_command:process_status_line(Data, Acc).

formulate_response(Response, Acc) -> eimap_command:formulate_response(Response, Acc).

