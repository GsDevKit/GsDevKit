testing
isOverride
	self theClass allSuperclassesDo: 
		[:class | (class includesSelector: self selector) ifTrue: [^ true]].
	^ false