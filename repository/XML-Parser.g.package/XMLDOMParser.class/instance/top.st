private
top
	^self stack isEmpty
		ifTrue: [nil]
		ifFalse: [self stack last]