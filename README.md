# GsDevKit

**Work in Progress** eventual replacement for glassdb/glass project

## Installation

```Smalltalk

Gofer new
  package: 'GsUpgrader-Core';
  repository: (MCGitHubRepository location: 'github://GsDevKit/gsUpgrader:dev/repository');
  load.
(Smalltalk at: #GsUpgrader) upgradeGsDevKit.
```
