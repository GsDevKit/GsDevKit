callbacks
commands
	| commands node buttons grouped groups |
	node _ self currentNode.
	node ifNil: [^ #()].
	commands _ (self announce: OBNodeCommandScan) commandsOn: node for: self.
	buttons _ commands select: [:ea | ea wantsButton].
	groups _ (buttons collect: [:ea | ea group]) asSet asSortedCollection: [:a :b | a > b].
	grouped _ groups collect: [:ea | (buttons select: [:b | b group = ea]) asSortedCollection: [:a :b | a order <= b order]].
	^ grouped gather: [:ea | ea]