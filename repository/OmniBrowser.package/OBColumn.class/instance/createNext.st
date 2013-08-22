updating
createNext
	| nextMetaNode created |
	nextMetaNode := self nextMetaNode.
	created := panel columnClass inPanel: panel metaNode: nextMetaNode node: self selectedNode.
	panel pushColumn: created.
	^created