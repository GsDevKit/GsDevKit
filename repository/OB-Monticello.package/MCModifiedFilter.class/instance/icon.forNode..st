as yet unclassified
icon: aSymbol forNode: aNode
	(aNode hasWorkingCopy not or: [aNode workingCopy isNil]) ifTrue: [^ #blank].
	
	^ aNode workingCopy modified
		ifTrue: [#breakpoint]
		ifFalse: [aNode workingCopy needsSaving
					ifTrue: [#arrowDown]
					ifFalse: [#blank]]