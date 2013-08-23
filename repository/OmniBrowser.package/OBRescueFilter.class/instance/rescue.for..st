private
rescue: aCollection for: aNode
	| previous |
	previous _ rescued at: aNode ifAbsent: [#()].
	^ rescued at: aNode put: previous, aCollection.