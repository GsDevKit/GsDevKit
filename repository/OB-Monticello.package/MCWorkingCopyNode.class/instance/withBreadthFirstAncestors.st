navigating
withBreadthFirstAncestors

	^self workingCopy ancestry withBreadthFirstAncestors collect: [:ea | ea asNode ]