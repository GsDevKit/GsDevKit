running
tearDown
	[(self class organization listAtCategoryNamed: self methodCategory)
		do: [ :each | self class removeSelector: each ifAbsent: []]] on: Error do: [:ignored |].
	(self class categoryNames includes: self methodCategory)
		ifTrue: [self class removeCategory: self methodCategory].