accessing
autoSelection
	| auto |
	auto := parent metaNode autoSelect.
	^auto 
		ifNotNil: [children detect: [:ea | ea metaNode == auto] ifNone: [nil]]