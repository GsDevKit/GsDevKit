*ob-standard
isScarySelector: newbieSelector

	"Return true if newbieSelector is already a part of Metaclass protocol."
	(Metaclass includesSelector: newbieSelector) ifTrue: [^ true].
	(Class includesSelector: newbieSelector) ifTrue: [^ true].
	(Behavior includesSelector: newbieSelector) ifTrue: [^ true].
	^ false