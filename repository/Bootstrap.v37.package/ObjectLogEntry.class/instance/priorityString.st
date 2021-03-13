displaying
priorityString

	| str |
	str := priority asString.
	self hasTag ifTrue: [ str := str, '*' ].
	^str