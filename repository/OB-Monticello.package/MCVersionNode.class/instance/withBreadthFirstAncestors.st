accessing
withBreadthFirstAncestors
	^self versionInfo withBreadthFirstAncestors collect: [:ea | ea asNode ]