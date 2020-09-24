instance creation
default

	| cl |
	cl := self onClient
		ifTrue: [ ClientFileDirectory ]
		ifFalse: [ ServerFileDirectory ].
	^cl new pathString: (GsFile _expandEnvVariable: 'PWD' isClient: self onClient); yourself