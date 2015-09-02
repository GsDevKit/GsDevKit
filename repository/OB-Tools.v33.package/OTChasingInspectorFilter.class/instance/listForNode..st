as yet unclassified
listForNode: aNode

	| l |
	l := aNode isNil
		ifTrue: [ Array new ]
		ifFalse: [ aNode filterPragmas collect: [ :pragma | (pragma argumentAt: 1) asString ] ].
	^l