as yet unclassified
includesSystemCategory: t1 
	^ self systemCategories
		anySatisfy: [:t2 | t2 sameAs: t1]