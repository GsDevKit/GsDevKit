as yet unclassified
with: aSearchNode
	^ self
		metaNode: self defaultMetaNode
		root: (OBCollectionNode on: (OrderedCollection with: aSearchNode))
		selection: aSearchNode