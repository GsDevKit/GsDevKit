*ob-monticello
obConfigureFromTemplate: aString
	| pathString |
	pathString := OBTextRequest prompt: 'Enter full path to directory on server' template: ServerFileDirectory default pathName.
	pathString == nil ifTrue: [ ^nil ]. 
	^self new directory: (ServerFileDirectory on: pathString)