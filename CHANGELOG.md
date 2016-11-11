# Changelog
All notable changes to this project will be documented in this file.

This project adheres to [Semantic Versioning](http://semver.org/).

## [Unreleased]
### Added
### Changed
### Deprecated
### Removed
### Fixed
### Security

## [0.4.0] - 2016-11-11
### Added
- optional timeout on eimap objects for server delay
  passing in an integer command_timeout value to eimap:start_link
  sets the timeout to that number of ms

### Fixed
- METADATA command parses multiple key/value returns correctly

## [0.3.0] - 2016-07-29
### Changed
- moved to rebar3

### Fixed
- consistency in capabilities response parsing

## [0.2.5] - 2016-07-04
### Fixed
- improved the capabilities response parsing

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

