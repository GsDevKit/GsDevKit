*ob-monticello
obConfigureFromTemplate: aString
	| chunk repo |
	chunk := OBMultiLineTextRequest 
		prompt: self fillInTheBlankRequest 
		template: (aString ifNil: [self creationTemplate]).
	chunk 
		ifNotNil: [ 
			chunk isEmpty ifTrue: [ ^nil ].
			repo := self readFrom: chunk readStream.
			repo creationTemplate: chunk. 
	].

	^ repo