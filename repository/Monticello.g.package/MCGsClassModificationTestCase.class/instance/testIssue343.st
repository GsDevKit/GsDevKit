tests
testIssue343
	"Force new class version (use two different defs so that repeated running of tests guarantees a new class version) and make sure that the classVar symbolDictionary still has a name ... Ensure that MCMockAPoolDictionary has been initialized correctly (G/S 2.4.4.x may have old state in mocks)"
	
	Object 
		subclass:  'Issue343TestClass' 
		instanceVariableNames: '' 
		classVariableNames: '' 
		poolDictionaries: 'MCMockAPoolDictionary' 
		category:  'Issue343Tests'.
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
