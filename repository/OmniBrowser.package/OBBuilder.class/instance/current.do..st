accessing
current: anObject do: aBlock

	| saved | 
	[saved := current.
	current := anObject.
	aBlock value]
		ensure: [current := saved].
	^ anObject