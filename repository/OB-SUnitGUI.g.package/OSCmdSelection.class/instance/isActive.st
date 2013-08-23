testing
isActive
	| columnIndex nodes |
	(requestor isKindOf: OSMultipleSelectionColumn) ifFalse: [ ^false ].
	columnIndex := requestor panel columns indexOf: requestor.
	((nodes := requestor selectedNodes) includes: target)
		ifTrue: [
			"active logic with selections"
 			columnIndex = 1 ifTrue: [ ^target isCategoryNode ].
			columnIndex = 2 ifTrue: [ ^target isTestNode ].
			columnIndex = 3 ifTrue: [ ^false ]]
		ifFalse: [
			"active logic with no selections"
			nodes isEmpty ifFalse: [ ^false ].
			^columnIndex ~~ 3 and: [target isTestCategoryNode or: [ target isTestNode or: [ target isTestRunnerNode ]]]]