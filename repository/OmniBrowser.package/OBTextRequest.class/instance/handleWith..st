dispatching
handleWith: anObject
	(ProvideAnswerNotification signal: self prompt) ifNotNilDo: [:answer |
		^answer == #default ifTrue: [ self template ] ifFalse: [ answer ]].
	^ anObject handleTextRequest: self