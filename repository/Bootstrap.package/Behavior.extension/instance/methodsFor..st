*bootstrap
methodsFor: categoryName 

	^ ClassCategoryReader new setClass: self
			category: categoryName asSymbol
			changeStamp: self changeStamp