private
validateMNU: exception receiver: receiver selector: selector messageArgs: messageArgs

	| args mArgs|
	self assert: exception class == MessageNotUnderstood.
	args := exception gsArguments.
	self assert: (args at: 1) == receiver.
	self assert: (args at: 2) == selector.
	mArgs := (args at: 3).
	self assert:  mArgs size == messageArgs size.
	1 to: mArgs size do: [:i | self assert: (mArgs at: i) == (messageArgs at: i) ].