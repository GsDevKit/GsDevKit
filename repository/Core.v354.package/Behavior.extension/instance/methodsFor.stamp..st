*core-squeak
methodsFor: categoryName stamp: aString 

	^ ClassCategoryReader new setClass: self
			category: categoryName asSymbol
			changeStamp: aString