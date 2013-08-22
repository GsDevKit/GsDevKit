filtering
icon: aSymbol forNode: aNode
	^ icon
		ifNil: [super icon: aSymbol forNode: aNode]
		ifNotNil: [icon value: aSymbol value: aNode]