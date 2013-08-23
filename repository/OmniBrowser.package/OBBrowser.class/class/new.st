instance creation
new
	^ self 
		metaNode: self defaultMetaNode
		root: self defaultRootNode
		selection: nil
		panels: self panels