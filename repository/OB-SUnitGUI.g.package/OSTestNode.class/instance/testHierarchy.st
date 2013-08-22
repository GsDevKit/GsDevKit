navigating
testHierarchy

	| set |
	set := Set new.
	self coParents do: [:aParent| 
		set addAll: ((self nodeHierarchyWithClass: OSTestNode) collect: [:ea | ea root: root ]) ].
	^set