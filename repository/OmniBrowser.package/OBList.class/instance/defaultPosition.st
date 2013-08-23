selecting
defaultPosition
	^ (position class = OBPosition)
		ifTrue: [ position ]
		ifFalse: [ position := OBPosition new index: 1 ]