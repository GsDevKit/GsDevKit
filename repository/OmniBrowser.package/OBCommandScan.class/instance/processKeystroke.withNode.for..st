user interface
processKeystroke: aCharacter withNode: aNode for: aRequestor
	| commands |
	commands _ self commandsOn: aNode for: aRequestor. 
	(commands select: [:cmd | cmd keystroke == aCharacter]) do:
		[:cmd |
		(cmd isActive and: [cmd isEnabled])
			ifTrue: [cmd execute]]