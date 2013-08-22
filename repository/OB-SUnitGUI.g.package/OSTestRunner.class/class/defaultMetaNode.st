configuration
defaultMetaNode
	| runner cats tests testCases |
	runner := OBMetaNode named: 'Runner'.
	runner addFilter: OSPatternFilter new.
	cats := OBMetaNode named: 'Categories'.
	cats addFilter: OSPatternFilter new.
	runner childAt: #categories put: cats.
	tests := OBMetaNode named: 'Test Cases'.
	cats childAt: #tests put: tests.
	cats addFilter: OSClassSortFilter new.
	testCases := OBMetaNode named: 'Defects'.
	tests childAt: #testCases put: testCases.
	testCases addFilter: OSTestCaseFilter new.
	^runner