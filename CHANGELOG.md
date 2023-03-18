# Changelog

All changes that impact users of this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

<!---
This document is intended for the users of astrolisp. Changes to things
like tests should not be noted in this document.
When updating this file for a PR, add an entry for your change under Unreleased
and one of the following headings:

 - Added - for new features.
 - Changed - for changes in existing functionality.
 - Deprecated - for soon-to-be removed features.
 - Removed - for now removed features.
 - Fixed - for any bug fixes.
 - Security - in case of vulnerabilities.
-->

## Changes

### Added
**Date:** *Feb 24th, 2023*
**Author:** *Anton Hibl*

Added a changelog to help users keep track of changes more clearly.

### Changed
**Date:** *Feb 26th, 2023*
**Author:** *Anton Hibl*

Changed gptai-model and gptai-api-key so that they are able to be set through
the customization menu

### Added 
**Date:** *Mar 17th, 2023*
**Author:** *Anton Hibl*


Added gptai-turbo which includes basic query functionality for the gpt-3.5-turbo
model; planned to be extended into a larger interface and chat interface
soon. Tests have been included in the tests directory for development purposes,
but they are not installed during package-install as they are intended solely
for development and not for end users. Including them would unnecessarily
increase the package size.
