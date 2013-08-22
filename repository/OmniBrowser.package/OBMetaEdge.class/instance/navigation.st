accessing
navigation
	^ navigation ifNil: [ navigation := OBDefaultEdgeNavigation new ]