install script
installScriptFor: aVersionString repositoryMap: repositoryMap on: stream
  | metacelloVersionString monticelloVersionString coreVersionString projectPackage |
  stream
    cr;
    nextPutAll: 'run';
    cr;
    nextPutAll: 'UserGlobals';
    cr;
    tab;
    nextPutAll: 'at: #BootstrapSymbolDictionaryName';
    cr;
    tab;
    nextPutAll:
        'ifAbsent: [ UserGlobals at: #BootstrapSymbolDictionaryName put: #UserGlobals ].';
    cr;
    nextPutAll: 'UserGlobals';
    cr;
    tab;
    nextPutAll: 'at: #BootstrapRepositoryDirectory';
    cr;
    tab;
    nextPutAll:
        'ifAbsent: [ UserGlobals at: #BootstrapRepositoryDirectory put: '
            , self defaultRepositoryDirectory , ' ].';
    cr;
    nextPutAll: 'UserGlobals';
    cr;
    tab;
    nextPutAll: 'at: #BootstrapUseRepositoryDirectory';
    cr;
    tab;
    nextPutAll:
        'ifAbsent: [ UserGlobals at: #BootstrapUseRepositoryDirectory put: '
            , self defaultUseRepositoryDirectory , ' ].';
    cr;
    nextPutAll: 'UserGlobals';
    cr;
    tab;
    nextPutAll: 'at: #BootstrapConfigurationClassName';
    cr;
    tab;
    nextPutAll:
        'ifAbsent: [ UserGlobals at: #BootstrapConfigurationClassName put: '
            , self configuration name asString printString , ' ].';
    cr;
    nextPutAll: 'UserGlobals';
    cr;
    tab;
    nextPutAll: 'at: #BootstrapConfigurationLoads';
    cr;
    tab;
    nextPutAll:
        'ifAbsent: [ UserGlobals at: #BootstrapConfigurationLoads put: #('.
  self loads do: [ :pkgName | stream nextPutAll: pkgName printString , ' ' ].
  stream
    nextPutAll: ') ].';
    cr;
    nextPutAll: 'true';
    cr;
    nextPut: $%;
    cr.
  stream
    cr;
    nextPutAll: 'commit';
    cr;
    cr.
  metacelloVersionString := ((self configuration project version: aVersionString)
    packageNamed: 'Metacello') versionString.
  monticelloVersionString := ((self configuration project
    version: aVersionString) packageNamed: 'Monticello') versionString.
  coreVersionString := ((self configuration project version: aVersionString)
    packageNamed: 'Core') versionString.
  projectPackage := self configuration project projectPackage.
  stream
    cr;
    nextPutAll:
        'input $upgradeDir/bootstrapMaster' , self extensionAndAttributes first first
            , '.topaz';
    cr;
    cr.
  stream
    cr;
    nextPutAll: 'run';
    cr;
    nextPutAll: '| driver |';
    cr;
    nextPutAll: 'driver := GLASSBootstrapDriver new.';
    cr;
    nextPutAll: 'UserGlobals at: #BootstrapDriver put: driver.';
    cr.
  stream
    nextPutAll: 'driver';
    cr;
    tab;
    nextPutAll:
        'bootStrapSymbolDictionary: (System myUserProfile symbolList objectNamed: BootstrapSymbolDictionaryName);';
    cr;
    tab;
    nextPutAll: 'repositoryDirectory: BootstrapRepositoryDirectory;';
    cr;
    tab;
    nextPutAll: 'useRepositoryDirectory: BootstrapUseRepositoryDirectory;';
    cr;
    tab;
    nextPutAll:
        'configurationPackageName: ' , projectPackage name printString , ';';
    cr;
    tab;
    nextPutAll:
        'configurationRepository: '
            , projectPackage repositorySpecs first name printString , ';';
    cr;
    tab;
    nextPutAll: 'configurationClass: BootstrapConfigurationClassName;';
    cr;
    tab;
    nextPutAll: 'configurationPackageLoads: BootstrapConfigurationLoads;';
    cr;
    tab;
    nextPutAll:
        'configurationVersionString: ' , aVersionString printString , ';';
    cr;
    tab;
    nextPutAll:
        'metacelloVersionString: ' , metacelloVersionString printString , ';';
    cr.
  stream
    tab;
    nextPutAll: 'repositoryMap: { ';
    cr.
  repositoryMap
    associationsDo: [ :assoc | 
      stream
        tab;
        tab;
        nextPutAll:
            assoc key externalReference version package name printString , ' -> {'.
      assoc value
        do: [ :repoSpec | stream nextPutAll: repoSpec description printString , '. ' ].
      stream
        nextPutAll: '}.';
        cr ].
  stream
    tab;
    nextPutAll: ' }.';
    cr;
    nextPutAll: 'true';
    cr;
    nextPut: $%;
    cr.
  stream
    cr;
    nextPutAll: 'commit';
    cr.
  self installConfigurationDoit isEmpty
    ifTrue: [ 
      stream
        cr;
        nextPutAll: '# standard configuration install';
        cr;
        nextPutAll: 'run';
        cr;
        nextPutAll: 'BootstrapDriver installConfigurationPackage.';
        cr;
        nextPutAll: 'true';
        cr;
        nextPut: $%;
        cr ]
    ifFalse: [ 
      stream
        cr;
        nextPutAll: '# custom configuration install';
        cr;
        nextPutAll: self installConfigurationDoit;
        cr ].
  stream
    cr;
    nextPutAll: 'run';
    cr;
    nextPutAll: 'BootstrapDriver installConfiguration.';
    cr;
    nextPutAll: 'SystemOrganizer resetSystemOrganization.';
    cr;
    nextPutAll: 'true';
    cr;
    nextPut: $%;
    cr.
  stream
    cr;
    nextPutAll: 'commit';
    cr.
  stream
    cr;
    nextPutAll: 'run';
    cr;
    nextPutAll: 'UserGlobals removeKey: #BootstrapUseRepositoryDirectory.';
    cr;
    nextPutAll: 'UserGlobals removeKey: #BootstrapSymbolDictionaryName.';
    cr;
    nextPutAll: 'UserGlobals removeKey: #BootstrapRepositoryDirectory.';
    cr;
    nextPutAll: 'UserGlobals removeKey: #BootstrapConfigurationLoads.';
    cr;
    nextPutAll: 'UserGlobals removeKey: #BootstrapConfigurationClassName.';
    cr;
    nextPutAll: 'UserGlobals removeKey: #BootstrapDriver.';
    cr;
    nextPutAll: 'true';
    cr;
    nextPut: $%;
    cr.
  self extensionAndAttributes first first = '30'
    ifTrue: [ 
      stream
        cr;
        nextPutAll: 'input $upgradeDir/patchMaster30.gs';
        cr;
        cr ].
  stream
    cr;
    nextPutAll: 'commit';
    cr