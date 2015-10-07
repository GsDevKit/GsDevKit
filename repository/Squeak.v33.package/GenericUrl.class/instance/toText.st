printing
toText
	| s |
	s _ WriteStream on: String new.
	s nextPutAll: self schemeName.
	s nextPut: $:.
	s nextPutAll: self locator.

	self fragment ifNotNil: [ s nextPut: $#.  s nextPutAll: self fragment ].

	^s contents