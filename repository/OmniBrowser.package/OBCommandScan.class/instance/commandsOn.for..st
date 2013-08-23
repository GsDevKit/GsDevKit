accessing
commandsOn: aNode for: aRequestor
	^ factories collect: [:ea | ea on: aNode for: aRequestor]