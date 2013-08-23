accessing
jumpTo: cNode 
	cNode 
		ifNil: 
			[self clearSelection.
			self changed: #list.
			panel clearAfter: self]
		ifNotNil: 
			[self listHolder noteChild: cNode.
			self selectSilently: cNode.
			panel selectionChangedIn: self.
			switch currentNode: cNode.
			self changed: #list]