*OB-Standard
asNode
	(self respondsTo: #asEnhancedNode) ifTrue: [^self perform: #asEnhancedNode].
	^OBMethodNode on: self