installing
unload
	| previousVersion |
	self isOverrideMethod ifTrue: [
		previousVersion := self scanForPreviousVersion].
	previousVersion isNil
		ifTrue: [self actualClass ifNotNil: [:class |
			self removeSelector: selector fromClass: class]]
		ifFalse: [
			"remove our override from history"
			self fileInVersion: previousVersion]