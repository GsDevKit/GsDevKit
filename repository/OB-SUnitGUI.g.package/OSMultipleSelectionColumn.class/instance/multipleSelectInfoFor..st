callbacks
multipleSelectInfoFor: aJadeServer

	| list |
	list := Array new.
	self multipleSelection keysAndValuesDo: [:key :value |
		list add: key printString; add: value printString ].
	^aJadeServer stringFromCollection: list