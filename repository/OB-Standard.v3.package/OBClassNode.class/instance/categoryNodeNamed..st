nodes
categoryNodeNamed: aString 
	^ (OBMethodCategoryNode on: aString inClass: theClass)
			symbolDictionary: self symbolDictionary;
			yourself