listing
systemCategories
	^ SystemOrganizer new categories select: [:cat | self includesSystemCategory: cat]