private
getSelector: aText
	| sel |
	Smalltalk 
		at: #Parser 
		ifPresent: [:cl | ^ cl new parseSelector: aText].
	"GemStone mechanism for getting the selector"
	^(sel := Behavior parseSelector: aText for: self theClass) == nil
		ifTrue: [ sel ]
		ifFalse: [sel asSymbol ]