This is a basic IMAP client implementation written in Erlang.

The API is not finalized, and is currently limited to the eimap module. Its
current and only recognized use case is software intended to interact with a
Kolab server environment which are written in Erlang. Expansion of that scope
is welcome through community participation. However, currently the following
disclaimer should be taken with seriousness:

    USE AT YOUR OWN RISK. THINGS WILL CHANGE.

The following is equally true, however:

    CONTRIBUTIONS WELCOME. USAGE WELCOME.

Usage
=====

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

    ImapServerArgs = #eimap_server_config{ host = "acme.com" },
    { ok, Imap } = eimap:start_link(ImapServerArgs),
    eimap_imap:starttls(),
    eimap_imap:login(Imap, self(), undefined, "username", "password"),
    eimap_imap:connect(Imap),
    eimap_imap:get_path_tokens(Imap, self(), get_path_tokens)

The eimap_server_config record is defined in eimap.hrl and allows one to set
host, port and TLS settings. For TLS, the following values are supported:

    * true: start a TLS session when opening the socket ("implicit TLS")
    * starttls: start TLS via the STARTTLS IMAP command auomagically
    * false: just open a regular connection; this is what you usually want, and
             the client will call eimap:starttls/3 when it wishes to switch to
             an encrypted connection

The starttls, login and even get_path_tokens commands will be
queued and sent to the IMAP server only when the connection has been established.
This prevents having to wait for connection signals and lets you write what you
intend to be executed with as few issues as possible.

Commands are executed in the order they are queued, and they follow a consistent
parametic pattern:

    * the first parameter is the eimap PID returned by eimap:start_link/1
    * the second parameter is the PID the response should be sent to as a message
    * the third parameter is a token to send back with the response, allowing users
      of eimap to track responses to specific commands; undefined is allowed and will
      surpress the use of a response token
    * .. additional parameters specific to the command, such as username and
      password in the case of login

Responses are sent as a normal message with the form:

    Response
    { Token, Resposne }

The Token is the resposne token provided as the third parameter and may be anything.
The Response depends on the given command, but will be a properly formed Erlang term
representing the content of the response.

Passthrough Mode
================
eimap supports a mode of operation which simply passes data between the user
and the imap server blindly. This is known as "passthrough" mode and can be
started and stopped with the start_passthrough/2 and stop_passthrough/1
functions.

Data is queued up for sending with the passthrough_data/2 function, and will be
sent to the server as soon as possible.

Responses are similarly sent back to the initiator of the passthrough mode
for dispatch in the form of { imap_server_response, Data } messages. The receiver
is the PID passed to start_passthrough/2 as the second parameter.

As the user is entirely responsible for the traffic and thereby the state
of the IMAP conenction during passthrough, exercise caution while using
this mode.

Any commands which are queued using eimap's command functions (logini/5,
logout/3, etc) will interupt passthrough to run those commands. Once the queued
commands have been cleared passthrough will restart auomatically.

Commands
========

Individual commands are implemented in individual modules in the src/commands/
directory, and this is the prefered mechanism for adding features to eimap.

The API for commands is defined in src/eimap_command.erl as a behavior. Commands
are expected to provide at least two functions:

    new(Args): create a command bitstring to be passed to the imap server
               Args is specific to the command, and some commands ignore this
               parameter
    parse(Data, Tag): parses a response from the imap server; the Tag is the
               IMAP command tag sent with the command and Data is the full
               Data buffer being process (possibly including tagged lines).

Data to be parsed may contain multiple lines, and in the case of commands with
large responses may only contain part of the response.

In the case of partial responses, parse/2 may return { more, fun/3, State }.
The State object allows preserving the parsing state and will be passed back to
fun/3 in addition to the Data and Tag parameters.

parse/2 (or the continuation fun/3) should return { fini, Result } when 
successfully completed, or { error, Reason } when it fails. Only once either a
fini or error tuple are returned will the eimap process move on to the next
command queued.

IMAP Utils
==========

eimap_utils provides a set of handy helpers for use when IMAP'ing your way
across the network. These include folder path and UID set extractors, IMAP
server response manipulators and misc utilities.

Testing
=======
Tests can be run with `make tests` or `rebar eunit`.

All new commands must be accompanied by tests in the test/ directory. Currently
testing is not remotely complete in coverage (a historical accident), and this
needs to be rectified over time. New tests welcome.

Contributing
============
Maintainer: Aaron Seigo <aseigo@kolabsystems.com>
Mailing list: devel@lists.kolab.com
Project page: https://git.kolab.org/tag/eimap/

You can find the list of open tasks on the project page's workboard. Anything
in the backlog is open to be worked on.
