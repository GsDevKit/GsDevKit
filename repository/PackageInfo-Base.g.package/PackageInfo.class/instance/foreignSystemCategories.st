listing
foreignSystemCategories
	^ SystemOrganizer new categories
		reject: [:cat | self includesSystemCategory: cat]