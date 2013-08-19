ancestry
allAncestorsDo: aBlock visitedAncestors: visitedAncestors

	self ancestors do: [ :each |
		(visitedAncestors includes: each) ifFalse: [
			visitedAncestors add: each.
			aBlock value: each.
			each allAncestorsDo: aBlock visitedAncestors: visitedAncestors ] ]