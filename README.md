# GLASS [![Build Status](https://travis-ci.org/glassdb/glass.png?branch=master)](https://travis-ci.org/glassdb/glass)

## Installation
In a fresh extent run the following:

```Smalltalk
"Upgrade GLASS to to 1.0-beta.9.3"
ConfigurationOfGLASS project updateProject.
GsDeployer
  deploy: [ (ConfigurationOfGLASS project version: '1.0-beta.9.3') load ].
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

