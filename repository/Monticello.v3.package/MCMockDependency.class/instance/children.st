accessing
children
	^ children
		collect: [:t1 | self class fromTree: t1]