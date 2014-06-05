# GLASS [![Build Status](https://travis-ci.org/glassdb/glass.png?branch=master)](https://travis-ci.org/glassdb/glass)

## Installation

```Smalltalk
"Upgrade GLASS to to 1.0-beta.9.1"
ConfigurationOfGLASS project updateProject.
GsDeployer
  deploy: [ (ConfigurationOfGLASS project version: '1.0-beta.9.1') load ].

"Install Metacello"
GsDeployer deploy: [
 Metacello new
  baseline: 'Metacello';
  repository: 'github://dalehenrich/metacello-work:master/repository';
  get.
 Metacello new
  baseline: 'Metacello';
  repository: 'github://dalehenrich/metacello-work:master/repository';
  load: 'ALL' ].

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

