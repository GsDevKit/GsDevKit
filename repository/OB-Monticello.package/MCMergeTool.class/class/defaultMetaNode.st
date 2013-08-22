as yet unclassified
defaultMetaNode
	| merger conflicts ops |
	merger := OBMetaNode named: 'VersionMerger'.
	conflicts := OBMetaNode named: 'Conflicts'.
	ops := OBMetaNode named: 'Operations'.
	merger childAt: #sortedConflicts put: conflicts.
	merger childAt: #sortedOperations put: ops.
	^ merger