This is a basic IMAP client implementation written in Erlang.

The API is not finalized, and is currently limited to the eimap module.

To use eimap from your imap application add the following line to your rebar
config:

    { eimap, "*", {git, "git://git.kolab.org/diffusion/EI/eimap.git" }

There is no need to start the eimap application as it does not have any process
or startup routines related to its usage. eimap does rely on lager being avilable,
however.

eimap Module
============
The eimap erlang module is the home of the primary API for the eimap library. It
is a gen_fsm and should be started as a process in the normal Erlang/OTP manner for
use. 

An eimap instance represents a single IMAP connection to a single IMAP server
and is stateful: commands that are started may change the selected folder, for
instance, and commands that are sent may be put into a command queue for subsequent
execution depeding on the current state of the connection.

Once started, an eimap process may be directed to connect to an imap server
and then start with functions such as fetching path tokens:

    { ok, Imap } = egara_imap:start_link(ImapServerArgs),
    egara_imap:connect(Imap),
    egara_imap:get_path_tokens(Imap, self(), get_path_tokens)

In the above code, the get_path_tokens command will be queued and sent to the
IMAP server only when the connection has been established.

ImapServerArgs is an eimap_server_config record which is defined in the
eimap.hrl file.

Commands
========

Individual commands are implemented in individual modules in the src/commands/
directory, and this is the prefered mechanism for adding features to eimap.

The API for commands is defined in src/eimap_command.erl as a behavior.


IMAP Utils
==========


Testing
=======
All new commands must be accompanied by tests in the test/ directory. Currently
testing is not remotely complete in coverage (a historical accident), and this
needs to be rectified over time. New tests welcome.
