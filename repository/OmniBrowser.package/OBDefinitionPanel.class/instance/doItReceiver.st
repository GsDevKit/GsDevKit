callbacks
doItReceiver
	^ self 
		withDefinitionDo: [:def | (def respondsTo: #doItReceiver) ifTrue: [def doItReceiver]]
		ifNil: [nil]