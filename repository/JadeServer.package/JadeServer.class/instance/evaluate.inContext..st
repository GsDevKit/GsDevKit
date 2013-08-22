menu messages
evaluate: aString inContext: anObject

	| result |
	result := aString evaluateInContext: anObject symbolList: GsSession currentSession symbolList.
	OBPlatform current doAutoCommit ifFalse: [ OBInformRequest message: 'Auto commit failed. You will need to abort and try again.' ].
	^result