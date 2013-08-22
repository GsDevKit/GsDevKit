accessing
addBlankIconsIfNecessary: aSymbol

	self items do: [:item |
		item isMenuLine ifFalse: [
			item icon == nil ifTrue: [ item icon: aSymbol ].
		].
	].