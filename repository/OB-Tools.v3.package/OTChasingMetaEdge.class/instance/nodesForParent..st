as yet unclassified
nodesForParent: aNode
	| newMeta |
	aNode isLastNode ifTrue: [ ^#() ].
	newMeta := OTChasingMetaNode new
		addFilter: OTChasingInspectorFilter new;
		yourself.
	aNode filterPragmas do: [:pragma |
		newMeta childAt: pragma selector labeled: (pragma argumentAt: 1) asString put: newMeta ].
	^ (aNode childInspectorNodes) do: [:ea | ea metaNode: newMeta. ea navigation: self navigation. ]