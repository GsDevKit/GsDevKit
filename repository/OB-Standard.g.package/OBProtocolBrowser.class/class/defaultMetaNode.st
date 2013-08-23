configuration
defaultMetaNode
	| protocol method class |
	class := OBMetaNode named: 'Class'.
	protocol := OBMetaNode named: 'Protocol'.
	method := OBMetaNode named: 'Method'.
	class 
		ancestrySelector: #isDescendantOfClass:;
		childAt: #allCategories put: protocol.
	protocol 
		ancestrySelector: #isDescendantOfMethodCat:;
		childAt: #allMethods put: method.
	method 
		ancestrySelector: #isDescendantOfMethod:;
		displaySelector: #nameWithClassNameInBrackets.
	^ class