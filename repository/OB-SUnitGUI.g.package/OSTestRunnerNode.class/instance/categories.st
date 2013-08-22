accessing
categories
	| visible |
	visible := Set new.
	self baseClass withAllSubclassesDo: [ :each |
		each category ~~ nil ifTrue: [
			visible add: each category ] ].
	^(Array streamContents: [ :stream |
		Smalltalk organization categories do: [ :each |
			(visible includes: each)
				ifTrue: [ stream nextPut: each ] ] ])
		collect: [:ea | 
			(OSTestCategoryNode on: self baseClass category: ea)
				root: self;
				yourself ]