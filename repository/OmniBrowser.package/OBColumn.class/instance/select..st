selecting
select: aNode 
	self listHolder children doWithIndex: [:child :i | 
		child = aNode ifTrue: [
			selection := nil. 
			^self selection: i]].
	self selection: nil