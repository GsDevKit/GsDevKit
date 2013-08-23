testing
canRecordUpdates

	canRecordUpdates == nil 
		ifTrue: [ canRecordUpdates := (ClientForwarder compiledMethodAt: #_asOop ifAbsent: [ nil ]) ~~ nil  ].
	^canRecordUpdates