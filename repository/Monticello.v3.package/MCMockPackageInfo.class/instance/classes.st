as yet unclassified
classes
	^ self classNames
		select: [:t1 | Smalltalk hasClassNamed: t1]
		thenCollect: [:t1 | Smalltalk at: t1]