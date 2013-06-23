Adding
tab
"Adds a tab to the output stream."

| tab |
tab := self isBinary
	ifTrue: [ Character tab asciiValue ]
	ifFalse: [ Character tab ].
self nextPut: tab.
