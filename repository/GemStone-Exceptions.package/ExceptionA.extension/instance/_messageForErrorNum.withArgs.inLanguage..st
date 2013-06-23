*gemstone-exceptions
_messageForErrorNum: aNumber withArgs: args inLanguage: aSymbol

	| errArray msgParts stream |
	errArray := GemStoneError at: aSymbol otherwise: nil. 
	errArray == nil ifTrue: [^nil].
	(aNumber < 1 or: [ aNumber > errArray size]) ifTrue: [^nil].
	msgParts := errArray at: aNumber.
	msgParts == nil ifTrue:[^nil].
	stream := WriteStream on: String new.
	msgParts do: [:each | 
		(each isKindOf: String) ifTrue: [
			stream nextPutAll: each.
		] ifFalse: [
			| object string |
			(each _isSmallInteger and: [each <= args size])
				ifTrue: [
					stream nextPut: $<.
					object := args at: each.
					string := object == self
						ifTrue: [ 'a ' , object class name ]
						ifFalse: [ [ object printString ] on: Error do: [:ex | ex return: 'a ' , object class name ]].
					stream nextPutAll: string.
					stream nextPut: $> ]
				ifFalse: [ stream nextPutAll: each asString ]]].
	^stream contents.
