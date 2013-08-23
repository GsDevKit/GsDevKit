navigating
showSenders: ann
	ann fan children size = 1 ifTrue: 
		[ann bid: 1 toDo: 
			[self class 
				openRoot: self class defaultRootNode
				selection: ann fan children first asMethodNode]]