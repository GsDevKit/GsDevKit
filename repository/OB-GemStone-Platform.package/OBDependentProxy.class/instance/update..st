updating
update: aSymbol

	self concentrator == nil ifTrue: [ super update: aSymbol ].
	^self concentrator update: aSymbol for: self