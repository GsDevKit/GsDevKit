# GLASS [![Build Status](https://travis-ci.org/glassdb/glass.png?branch=master)](https://travis-ci.org/glassdb/glass)

## Installation
In a fresh extent run the following:

```Smalltalk
"Upgrade Metacello and Grease first"
Gofer new
  package: 'GsUpgrader-Core';
  url: 'http://ss3.gemtalksystems.com/ss/gsUpgrader';
  load.
(Smalltalk at: #GsUpgrader) upgradeGrease.

"Install GLASS from github"
GsDeployer deploy: [
 Metacello new
  baseline: 'GLASS1';
  repository: 'github://glassdb/glass:master/repository';
  get.
 Metacello new
  baseline: 'GLASS1';
  repository: 'github://glassdb/glass:master/repository';
  onConflict: [ :ex | ex allow ];
  onWarning: [ :ex | 
        Transcript
          cr;
          show: ex description.
        ex resume ];
  load: 'default' ].
```

