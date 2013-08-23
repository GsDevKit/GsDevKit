navigating
selectAncestorsOf: aNode 
	| subtree |
	subtree := OBSubtree from: root to: aNode.
	subtree ifNotNil: [self selectSubtree: subtree]