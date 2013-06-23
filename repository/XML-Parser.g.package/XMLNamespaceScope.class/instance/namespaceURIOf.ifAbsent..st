accessing
namespaceURIOf: ns ifAbsent: aBlock
	"Retrieve the URI of the given namespace prefix, if it is defined. 
	A nil namespace returns the default namespace. 
	If no namespace can be found the value of the block is returned"

	^ns
		ifNil: [self defaultNamespace]
		ifNotNil: [currentBindings at: ns ifAbsent: aBlock]