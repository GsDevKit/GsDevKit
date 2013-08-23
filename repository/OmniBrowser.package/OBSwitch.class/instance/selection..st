as yet unclassified
selection: anInteger
	filter ifNotNil: 
		[filter selection: anInteger.
		self changed: #selection.
		column listChanged]