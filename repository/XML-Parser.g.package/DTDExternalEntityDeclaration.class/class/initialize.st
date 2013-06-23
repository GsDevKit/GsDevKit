class initialization
initialize
	"DTDExternalEntityDeclaration initialize"

	contextBehavior _ Dictionary new.
	contextBehavior
		at: #content put: #include ;
		at: #attributeValueContent put: #includedInLiteral ;
		at: #attributeValue put: #forbidden ;
		at: #entityValue put: #bypass ;
		at: #dtd put: #forbidden