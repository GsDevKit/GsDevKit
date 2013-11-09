ancestry
isDescendantOfMethod: other
	^ other selector = selector
		and: [self theClass withAllSuperclasses includes: other theClass].
	