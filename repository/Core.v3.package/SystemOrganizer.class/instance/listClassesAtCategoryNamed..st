accessing
listClassesAtCategoryNamed: cat
	^(self categoryDict at: cat asString otherwise: [#()]) collect: [:each| each]