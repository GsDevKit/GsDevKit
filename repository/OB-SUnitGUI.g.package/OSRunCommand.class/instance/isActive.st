testing
isActive
	"run commands only active if a selection has been made in 2nd column"
	
	(requestor isKindOf: OBFixedButtonPanel) ifTrue: [ ^true ].
	(requestor isKindOf: OSMultipleSelectionColumn) ifFalse: [ ^false ].
	^((requestor panel columns indexOf: requestor) = 2) and: [ target isTestNode ]