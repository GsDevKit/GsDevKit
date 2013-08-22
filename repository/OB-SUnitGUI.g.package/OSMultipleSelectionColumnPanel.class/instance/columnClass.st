constructing
columnClass
	columns size >= self maxMultipleSelectionPanes ifTrue: [ ^super columnClass ].
	^OSMultipleSelectionColumn