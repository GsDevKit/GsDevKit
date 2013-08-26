accessing
removeElement: element 

	| catName ar |
	catName := self categoryOfElement: element.
	catName == nil ifTrue: [ ^self ].
	ar := self categoryDict at: catName asString.
	ar remove: element ifAbsent: [].