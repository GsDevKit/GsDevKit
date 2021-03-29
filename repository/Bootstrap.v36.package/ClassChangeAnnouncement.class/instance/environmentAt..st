private
environmentAt: anItemKind

	anItemKind == #class ifTrue: [ ^self item ].
	^nil