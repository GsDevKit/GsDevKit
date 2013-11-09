accessing
versionsOfMethod: methodReference 
	| class selector |
	class := methodReference actualClass.
	selector := methodReference methodSymbol.
	^ self versionsOfMethod: selector in: class
