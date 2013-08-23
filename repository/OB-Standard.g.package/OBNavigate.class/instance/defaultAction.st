as yet unclassified
defaultAction
	^ [fan children size = 0 
		ifTrue: [OBInformRequest message: self noChildrenMessage]
		ifFalse: [self browserClass openRoot: fan parent]]