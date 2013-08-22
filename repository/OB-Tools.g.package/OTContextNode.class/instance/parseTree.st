accessing-dynamic
parseTree
	^ parseTree ifNil: [ parseTree := super parseTree ]