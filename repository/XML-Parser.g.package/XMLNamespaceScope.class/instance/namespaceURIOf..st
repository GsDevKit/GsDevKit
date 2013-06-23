accessing
namespaceURIOf: ns
	"Retrieve the URI of the given namespace prefix, if it is defined. A nil namespace
	returns the global namespace"

	^ self namespaceURIOf: ns ifAbsent: [ nil ]