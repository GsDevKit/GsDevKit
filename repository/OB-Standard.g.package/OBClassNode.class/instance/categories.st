navigating
categories
	^ self theClass sortedCategoryNames
		collect: [:cat | OBMethodCategoryNode on: cat asSymbol inClass: self theClass]