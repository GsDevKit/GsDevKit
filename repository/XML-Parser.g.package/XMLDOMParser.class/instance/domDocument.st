parsing
domDocument
	[self startDocument; parseDocument]
		ensure: [self driver stream close].
	^document