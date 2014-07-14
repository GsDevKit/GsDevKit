tests
testIssue343
	"Force new class version (use two different defs so that repeated running of tests guarantees a new class version) and make sure that the classVar symbolDictionary still has a name ... SharedPool has to hve been used as a pool already (see MCMockClassB)"
	
	self assert: (Smalltalk at: #'MCMockAPoolDictionary') _classVars name == #'MCMockAPoolDictionary'.
	SharedPool subclass: 'MCMockAPoolDictionary'
		instVarNames: #( yyy zzz)
		classVars: #()
		classInstVars: #()
		poolDictionaries: {}
		inDictionary: ''
		category: 'Monticello-Mocks'.
	self assert: (Smalltalk at: #'MCMockAPoolDictionary') _classVars name == #'MCMockAPoolDictionary'.
	SharedPool subclass: 'MCMockAPoolDictionary'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: {}
		inDictionary: ''
		category: 'Monticello-Mocks'.
	self assert: (Smalltalk at: #'MCMockAPoolDictionary') _classVars name == #'MCMockAPoolDictionary'.
