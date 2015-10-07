accessing-dynamic
method
	^ self theClass ifNotNilDo: [ :class | class lookupSelector: self selector ]