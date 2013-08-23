accessing
name
	^ isMeta
		ifTrue: [name, ' class']
		ifFalse: [name]