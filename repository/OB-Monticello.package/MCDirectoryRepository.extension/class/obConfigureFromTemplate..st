*ob-monticello
obConfigureFromTemplate: aString
	| pathString |
	pathString := OBTextRequest prompt: 'Enter full path to directory' template: FileDirectory default pathName.
	pathString == nil ifTrue: [ ^nil ]. 
	^self new directory: (FileDirectory on: pathString)