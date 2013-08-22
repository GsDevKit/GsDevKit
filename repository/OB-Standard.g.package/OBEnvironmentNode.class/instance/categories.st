navigating
categories
	^ environment systemOrganization categories collect: [:cat | OBClassCategoryNode 
															on: cat
															inEnvironment: environment]