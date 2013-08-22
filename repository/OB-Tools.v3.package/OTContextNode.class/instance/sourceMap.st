accessing-dynamic
sourceMap
	^ sourceMap ifNil: [ sourceMap := super sourceMap ]