accessing
on: aNode for: aRequestor
	| cmd |
	cmd := super on: aNode for: aRequestor.
	[ cmd setCommand: (command on: aNode methodNode for: aRequestor) ] on: Error do: [:ex | ^cmd ].
	^cmd