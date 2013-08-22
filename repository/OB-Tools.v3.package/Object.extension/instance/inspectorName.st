*ob-tools-inspector
inspectorName

	| ws |
	ws := PrintStream printingOn: String new maxSize: 100.
	self printOn: ws.
	^ws contents