Accessing
value: anObject
	| dict |
  	value := anObject.
	dict := self _transientDictionary.
	dict at: self put: anObject.