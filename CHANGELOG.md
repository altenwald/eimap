# Changelog
All notable changes to this project will be documented in this file.

This project adheres to [Semantic Versioning](http://semver.org/).

## [Unreleased]
### Added
- binding to a network interface (rather than an IP/host) with net_iface
### Changed
- upgrade build system to rebar3
### Deprecated
### Removed
### Fixed
### Security


## [0.2.4] - 2016-06-08
This section contains the changes from 0.2.0 through 0.2.4
### Added
- NOOP command
- support for automated interruption of passthrough state to send structured commands
- commands receive server responses for commands they put into the queue

### Changed
- centralize core IMAP response handling and utils in eimap_command
- support for multi-line, single-line and binary response command types
- improved TLS support

### Fixed
- crash in GETMETADATA command when the Folder param is a list
- Support more variations of the LIST command args in the filter_groupware rule
- Prevent crashes (while maintaining simplicity) in session FSM by limiting
  processcommandqueue messages in the mailbox to one
- support for literals continuation
- fixes for metadata fetching

