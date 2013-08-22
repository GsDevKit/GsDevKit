filtering
filterProtocols: protocols onClasses: allClasses forNode: aNode 
	| filtered |
	filtered := (protocols allButFirst "the first is the synthetic --all-- category"
			sortWithBlock: [:x :y | x category < y category ]) 
		select: [:p | allClasses anySatisfy: [:c | c categoryNames includes: p name]].
	filtered := filtered asOrderedCollection.
	filtered addFirst: protocols first.
	^filtered asArray