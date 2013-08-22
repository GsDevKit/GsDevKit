comparing
= other 
	^ self species = other species and: [ self theClass = other theClass and: [ self selector = other selector ] ]