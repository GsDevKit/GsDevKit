scope
leaveScope
	| leftScope |
	leftScope := self scope removeLast.
	currentBindings := (self currentScope at: 2) ifNil: [leftScope at: 3]