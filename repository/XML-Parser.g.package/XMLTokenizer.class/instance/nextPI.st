tokenizing
nextPI
	| piTarget piData |
	"Skip ?"
	self next.
	piTarget _ self nextLiteral.
	piTarget asUppercase = 'XML'
		ifTrue: [^self nextXMLDecl].
	self skipSeparators.
	piData _ self nextUpToAll: '?>'.
	self handlePI: piTarget data: piData