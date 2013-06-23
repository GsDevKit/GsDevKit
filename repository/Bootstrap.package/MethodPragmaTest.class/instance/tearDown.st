running
tearDown
	(self class organization listAtCategoryNamed: self methodCategory)
		do: [ :each | self class removeSelector: each ifAbsent: []].
	(self class categoryNames includes: self methodCategory)
		ifTrue: [self class removeCategory: self methodCategory].