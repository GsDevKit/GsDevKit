filtering
nodesFrom: nodes forNode: parent 
	"Sort nodes according to their position in the class hierarchy"
	| classes classNodes sortedNodes classInCenter supersChain |
	classNodes := nodes select: [ :ea | ea respondsTo: #theClass ].
	classes := (classNodes collect: [ :classNode | classNode theClass ]) asOrderedCollection.
	classes ifEmpty: [ ^ nodes ].
	self initializeResults.
	classNodes do: 
		[ :classNode | 
		supersChain := classNode theClass withAllSuperclasses reversed.
		supersChain removeAllSuchThat: [ :cl | (classes includes: cl) not ].
		order add: supersChain contents -> classNode ].
	self buildIndentIndex.
	^ self collectSortedNodes