testing
testRevertOldMethod
	| definition changeRecord |
	MCMockObject compile: 'yourself ^ self' classified: self mockOverrideMethodCategory.
	definition := (MethodReference class: MCMockObject selector: #yourself) asMethodDefinition.
	changeRecord := definition scanForPreviousVersion.
	self assert: changeRecord notNil.
	self assert: changeRecord category = #'accessing'.
	definition fileInVersion: changeRecord.