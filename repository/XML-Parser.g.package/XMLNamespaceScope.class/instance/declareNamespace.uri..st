scope
declareNamespace: ns uri: uri
	"Declare the given name space prefix with the given URL"

	ns = 'xmlns'
		ifTrue: [^self defaultNamespace: uri].
	self establishLocalBindings.
	currentBindings removeKey: ns ifAbsent: [].
	currentBindings at: ns put: uri