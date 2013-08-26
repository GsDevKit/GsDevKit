as yet unclassified
methods
	^ MCMockClassA selectors
		select: [:t1 | t1 beginsWith: 'ordinal']
		thenCollect: [:t1 | MethodReference new setStandardClass: MCMockClassA methodSymbol: t1]