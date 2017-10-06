accessing
next

	| obj |
	obj := super next.
	self isBinary ifTrue: [ obj := obj asInteger ].
	^obj