navigating
packagesGroupedBy: aSelector
	
	^ (self packageDictGroupedBy: aSelector) values asSortedCollection: [:a :b | a name <= b name]