# GsDevKit[![Build Status](https://github.com/GsDevKit/GsDevKit/actions/workflows/ci.yml/badge.svg?branch=master)](https://github.com/GsDevKit/GsDevKit/actions/workflows/ci.yml)

**Work in Progress** eventual replacement for glassdb/glass project

## Installation

```Smalltalk

Gofer new
  package: 'GsUpgrader-Core';
  repository: (MCGitHubRepository location: 'github://GsDevKit/gsUpgrader:dev/repository');
  load.
(Smalltalk at: #GsUpgrader) upgradeGsDevKit.
```
