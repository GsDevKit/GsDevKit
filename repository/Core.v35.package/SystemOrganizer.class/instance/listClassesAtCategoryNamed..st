accessing
listClassesAtCategoryNamed: cat
	^(self categoryDict at: cat asSymbol otherwise: [#()]) collect: [:each| each]