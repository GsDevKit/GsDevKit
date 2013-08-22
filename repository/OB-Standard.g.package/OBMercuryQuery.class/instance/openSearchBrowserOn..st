actions
openSearchBrowserOn: searchNode 
	^ self browser isSearchBrowser 
		ifFalse: [(OBSearchBrowser with: searchNode) open]
		ifTrue: 
			[self browser root addChild: searchNode.
			(self announce: OBAboutToChange) isVetoed 
				ifTrue: [self browser noteChanges]
				ifFalse: [self announce: (OBSelectingNode node: searchNode)]]