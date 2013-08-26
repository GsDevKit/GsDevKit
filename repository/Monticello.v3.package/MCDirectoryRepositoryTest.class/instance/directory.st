as yet unclassified
directory
	directory ifNil:
		[directory := FileDirectory default directoryNamed: 'mctest'.
		directory assureExistence].
	^ directory