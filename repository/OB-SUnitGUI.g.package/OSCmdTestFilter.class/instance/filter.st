private
filter

	| node |
	node := requestor browser selectionPath detect: [:ea | ea isTestCategoryNode] ifNone: [ ^nil ].
	^node metaNode filtersOS detect: [:ea | ea isKindOf: OSPatternFilter ].