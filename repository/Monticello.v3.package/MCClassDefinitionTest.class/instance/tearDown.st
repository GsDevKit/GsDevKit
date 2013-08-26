as yet unclassified
tearDown

	super tearDown.
	(SystemOrganizer new categories includes: self issue285MockCategoryName)
		ifTrue: [SystemOrganizer new removeCategory: self issue285MockCategoryName].
	Smalltalk at: 'MCMockClassC' ifPresent: [:c | c removeFromSystem]