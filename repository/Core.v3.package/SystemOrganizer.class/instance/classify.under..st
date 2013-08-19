accessing
classify: element under: heading
	"Store the argument, element, in the category named heading."

	| realHeading catName ar |
	heading == nil
		ifTrue: [ realHeading := Default asString ]
		ifFalse: [ realHeading := heading asString ].
	(catName := self categoryOfElement: element) = realHeading ifTrue: [ ^self ].
	catName ~~ nil
		ifTrue: [
			realHeading = Default ifTrue: [ ^self ].
			self removeElement: element
		].
	ar := self categoryDict at: realHeading ifAbsent: [ nil ].
	ar == nil 
		ifTrue: [
			ar := Array new.
			self categoryDict at: realHeading put: ar.
		].
	ar add: element.