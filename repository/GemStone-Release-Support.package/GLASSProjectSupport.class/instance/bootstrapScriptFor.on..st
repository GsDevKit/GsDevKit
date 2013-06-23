install script
bootstrapScriptFor: aVersionString on: stream

	| metacelloVersionString monticelloVersionString coreVersionString goferVersionString goferProject |

	metacelloVersionString := ((self configuration project version: aVersionString) packageNamed: 'Metacello') versionString.
	monticelloVersionString := ((self configuration project version: aVersionString) packageNamed: 'Monticello') versionString.
	coreVersionString := ((self configuration project version: aVersionString) packageNamed: 'Core') versionString.
	goferProject := (ConfigurationOfMetacello project version: metacelloVersionString) packageNamed: 'Gofer Core'.
	goferProject ifNil: [ 
		"in Metacello 1.0-beta.32, Gofer project changed name to 'Gofer'"
		goferProject := (ConfigurationOfMetacello project version: metacelloVersionString) packageNamed: 'Gofer'].
	goferVersionString := goferProject versionString.

	stream 
		cr; nextPutAll: 'run'; cr;
		nextPutAll: '| undef |'; cr;
		nextPutAll: 'undef := SymbolDictionary new.'; cr;
		nextPutAll: 'undef name: #UndefinedSymbols.'; cr;
		nextPutAll: 'UserGlobals at: #UndefinedSymbols put: undef.'; cr;
		nextPutAll: 'UserGlobals at: #UndefinedSymbolList put: (SymbolList with: undef).'; cr;
		nextPutAll: 'true'; cr.
	stream
		nextPut: $%; cr; cr.

	stream 
		cr; nextPutAll: 'run'; cr;
		nextPutAll: 'GsPackagePolicy current enable.'; cr;
		nextPutAll: 'true'; cr.
	stream
		nextPut: $%; cr; cr.

	stream 
		cr; nextPutAll: 'run'; cr;
		nextPutAll: '| packageNames |'; cr;
		nextPutAll: 'packageNames := { '; cr.
	((self corePackageVersionsFor: coreVersionString), 
		(self monticelloPackageVersionsFor: monticelloVersionString), 
		(self goferPackageVersionsFor: goferVersionString),
		(self metacelloPackageVersionsFor: metacelloVersionString)) do: [:filename |
			stream tab; nextPutAll: (filename, '.mcz') printString, '.'; cr].
	stream 
		tab; nextPutAll: '}.';cr;
		nextPutAll: 'packageNames do: [:packageName |'; cr;
		tab; nextPutAll: 'GsPackageLibrary loadMCZFile: packageName fromRepositoryPath: BootstrapRepositoryDirectory ].';cr;
		nextPutAll: 'true'; cr.
	stream
		nextPut: $%; cr.
	stream
		cr; nextPutAll: 'commit'; cr; cr.

