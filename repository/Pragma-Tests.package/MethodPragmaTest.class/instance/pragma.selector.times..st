utilities
pragma: aSymbol selector: aSelector times: anInteger
	^ (self 
		compile: (String streamContents: [ :stream | 
			(self shuffle: (1 to: anInteger) asArray by: self randomForPicking)
				do: [ :each | 
					stream 
						nextPut: $<; nextPutAll: aSymbol; space;
						print: each; nextPut: $>; cr ] ])
		selector: aSelector)
			pragmas.
