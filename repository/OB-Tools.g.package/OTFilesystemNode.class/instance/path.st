accessing
path
	^ self isRoot
		ifTrue: [ OrderedCollection with: self ]
		ifFalse: [ self parent path addLast: self; yourself ]