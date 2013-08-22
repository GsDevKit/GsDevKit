configuration
defaultMetaNode
	| operations  patch |
	patch := OBMetaNode named: 'Patch'.
	operations := OBMetaNode named: 'Operations'.
	patch - #sortedOperations -> operations.
	^ patch