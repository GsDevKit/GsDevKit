listing
classes
	^(self systemCategories gather:
		[:cat |
		(SystemOrganizer new listAtCategoryNamed: cat)
			collect: [:className | Smalltalk classNamed: className]])
				sortBy: [:a :b | a className <= b className]