private
environmentAt: anItemKind

	anItemKind == #method ifTrue: [ ^self item ].
	anItemKind == #class ifTrue: [ ^self itemClass ].
	anItemKind == #selector ifTrue: [ ^self itemSelector ].
	^nil