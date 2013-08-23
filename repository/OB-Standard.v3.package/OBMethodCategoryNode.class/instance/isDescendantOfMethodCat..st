ancestry
isDescendantOfMethodCat: other
	^ (other theClassName = self theClassName) and: [other name = name]