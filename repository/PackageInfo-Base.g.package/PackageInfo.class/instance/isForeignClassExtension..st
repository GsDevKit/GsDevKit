testing
isForeignClassExtension: categoryName
	^ categoryName isEmpty not and: [ categoryName first = $* and: [(self isYourClassExtension: categoryName) not]]