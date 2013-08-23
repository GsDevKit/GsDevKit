callbacks
doItContext
	^ self 
		withDefinitionDo: [:def | (def respondsTo: #doItContext) ifTrue: [def doItContext]]
		ifNil: [nil]