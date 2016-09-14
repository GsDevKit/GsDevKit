utilities
pragma: aSymbol selector: aSelector times: anInteger
	^ (self 
		compile: (String streamContents: [ :stream | 
			((1 to: anInteger) asArray shuffledBy: self randomForPicking) do: [ :each | 
				stream 
					nextPut: $<; nextPutAll: aSymbol; space;
					print: each; nextPut: $>; cr ] ])
		selector: aSelector)
			pragmas.