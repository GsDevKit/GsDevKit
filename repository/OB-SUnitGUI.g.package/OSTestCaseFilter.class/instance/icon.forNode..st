filtering
icon: aSymbol forNode: aNode
	| status |
	status := aNode status.
	status == #error
		ifTrue: [ ^#osTestRed ].
	status == #failure
		ifTrue: [ ^#osTestYellow ].
	 ^ #blank