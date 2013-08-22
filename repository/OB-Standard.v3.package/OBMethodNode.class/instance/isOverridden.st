testing
isOverridden
	self theClass allSubclassesDo: 
		[:class | (class includesSelector: self selector) ifTrue: [^ true]].
	^ false