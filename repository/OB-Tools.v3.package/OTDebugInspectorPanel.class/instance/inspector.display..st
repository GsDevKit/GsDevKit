updating
inspector: anInspector display: anObject
	| col |
	anInspector navigationPanel root
		object: anObject.
	(col := anInspector navigationPanel currentColumn) ~~ nil 
		ifTrue: [ col refreshAndSignal: true ]
		ifFalse: [ anInspector signalRefresh]