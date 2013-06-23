tokenizing
nextAttributeInto: attributes namespaces: namespaces

	| attrName attrValue |
	attrName _ self nextName.
	self skipSeparators.
	self next == $=
		ifFalse: [self errorExpected: '='].
	self skipSeparators.
	attrValue _ self nextAttributeValue.

	(self usesNamespaces
		and: [(attrName findString: 'xmlns') = 1])
		ifTrue: [attrName size > 6
			ifTrue: [namespaces at: (attrName copyFrom: 7 to: attrName size) put: attrValue]
			ifFalse: [namespaces at: attrName put: attrValue]]
		ifFalse: [attributes at: attrName put: attrValue]