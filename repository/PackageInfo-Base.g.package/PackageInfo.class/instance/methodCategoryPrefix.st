naming
methodCategoryPrefix
	^ methodCategoryPrefix ifNil: [methodCategoryPrefix _ '*', self packageName asLowercase]