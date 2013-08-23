callbacks
selectedClass
	^ self 
		withDefinitionDo: [:def | (def respondsTo: #selectedClass) ifTrue: [def selectedClass]] 
		ifNil: [nil]