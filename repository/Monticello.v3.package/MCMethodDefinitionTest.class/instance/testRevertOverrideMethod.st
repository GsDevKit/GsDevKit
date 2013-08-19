testing
testRevertOverrideMethod
	| definition |
	self class compile: 'override ^ 2' classified:  '*foobarbaz-override'.
	definition := (MethodReference class: self class selector: #override) asMethodDefinition.
	self assert: definition isOverrideMethod.
	self assert: self override = 2.
	definition unload.
	self assert: self override = 1.
	self assert: (MethodReference class: self class selector: #override) category asString = 'mocks'.
	