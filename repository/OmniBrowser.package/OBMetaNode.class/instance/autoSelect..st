children
autoSelect: aMetanode
	autoSelect _ edges detect: [:ea | ea metaNode == aMetanode] ifNone: [nil]