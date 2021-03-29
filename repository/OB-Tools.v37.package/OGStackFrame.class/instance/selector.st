accessing
selector

	self method == nil ifTrue: [^''].
	self method selector == nil ifTrue: [^'Executed Code'].
	^self method selector