column
columnAfter: aColumn selection: anInteger 
	^ (self childAt: anInteger) 
	       ifNil: [nil] 
	       ifNotNilDo: [:node | node columnAfter: aColumn withFan: self selection: anInteger]