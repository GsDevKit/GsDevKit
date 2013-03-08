Transcript cr; show: 'travis--->before_gemstone.st'.
Transcript cr; show: 'GLASS: ', ConfigurationOfGLASS project currentVersion versionString.

ConfigurationOfMetacello project currentVersion versionNumber < '1.0-beta.31.1' asMetacelloVersionNumber 
    ifTrue: [
        "load basic Metacello packages for version 1.0-beta.31.1. 
	 To load GLASS 1.0-beta.8.7.4:
	  - Metacello must handle symbolic versions
	  - MetacelloSkipDirtyPackageLoad needs to be defined"
	Transcript cr; show: 'load basic Metacello packages for version 1.0-beta.31.1'.
	((Smalltalk at: #Gofer) new)
		gemsource: 'metacello';
		version: 'Metacello-Base-DaleHenrichs.19';
		version: 'Metacello-Core-dkh.468';
		version: 'Metacello-MC-dkh.531';
		load.
	((Smalltalk at: #Gofer) new)
		gemsource: 'metacello';
		version: 'Metacello-Platform.gemstone-dkh.23';
		load.
	(Smalltalk at: #MetacelloGemStonePlatform) initialize ]
