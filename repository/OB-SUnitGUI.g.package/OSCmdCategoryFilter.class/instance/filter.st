private
filter

	| node |
	node := requestor browser root.
	^node metaNode filtersOS detect: [:ea | ea isKindOf: OSPatternFilter ].