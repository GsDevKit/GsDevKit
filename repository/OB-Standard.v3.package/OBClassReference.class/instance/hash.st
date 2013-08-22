comparing
hash
	^ isMeta
		ifTrue: [name hash bitInvert]
		ifFalse: [name hash]