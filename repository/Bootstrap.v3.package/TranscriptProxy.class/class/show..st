transcript
show: anObject

	| proxy |
	(ObjectLogEntry transcript: anObject printString object: anObject) addToLog.
	(proxy := self proxyTranscript) ~~ nil 
		ifTrue: [ proxy show: anObject ]
		ifFalse: [ GsFile gciLogServer: '--transcript--', anObject printString ].