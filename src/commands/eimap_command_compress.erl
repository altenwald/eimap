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

-module(eimap_command_compress).
-behavior(eimap_command).
-export([new_command/1, formulate_response/2]).

%% http://tools.ietf.org/html/rfc2342

%% Public API
new_command(_Args) -> { <<"COMPRESS DEFLATE">>, single_line_response }.

formulate_response(Data, Tag) ->
    case eimap_utils:check_response_for_failure(Data, Tag) of
        ok -> compression_active;
        { _, Reason } -> { error, Reason }
    end.

