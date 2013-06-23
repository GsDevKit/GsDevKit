*squeak
allSubInstancesDo: aBlock 
	"Evaluate the argument, aBlock, for each of the current instances of the 
	receiver and all its subclasses."

	MCPlatformSupport autoCommit ifTrue: [ System commitTransaction ifFalse: [ self error: 'Auto commit failed' ]].
	self allInstances do: aBlock.
	self allSubclasses do: [:sub | 
		MCPlatformSupport autoCommit ifTrue: [ System commitTransaction ifFalse: [ self error: 'Auto commit failed' ]].
		sub allInstances do: aBlock]