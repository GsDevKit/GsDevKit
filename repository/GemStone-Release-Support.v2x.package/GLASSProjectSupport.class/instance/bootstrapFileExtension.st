accessing
bootstrapFileExtension

	^((System stoneVersionAt: 'gsVersion') beginsWith: '2.')
		ifTrue: [ '2' ]
		ifFalse: [ '3' ]