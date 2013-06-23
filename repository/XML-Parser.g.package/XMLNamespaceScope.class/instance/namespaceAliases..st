private
namespaceAliases: namespace
	"Locate all namespaces that are aliases of the given URI."

	| aliases uri |
	aliases _ Set new.
	uri _ self namespaceURIOf: namespace ifAbsent: [self parseError: 'Attribute refers to undefined namespace ' , namespace asString ].
	currentBindings keysAndValuesDo: [:ns :u |
		(u = uri
			and: [ns ~= namespace])
			ifTrue: [aliases add: ns]].
	^ aliases