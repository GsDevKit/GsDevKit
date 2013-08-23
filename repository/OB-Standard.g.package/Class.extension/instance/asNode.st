*ob-standard-converting
asNode
	(self respondsTo: #asEnhancedNode) ifTrue: [^self perform: #asEnhancedNode].
	^ OBClassNode on: self