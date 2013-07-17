testing
isYourClassExtension: categoryName
	^ categoryName notNil and: [self category: categoryName matches: self methodCategoryPrefix]