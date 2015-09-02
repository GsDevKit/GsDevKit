configuration
defaultMetaNode
	| directory file |
	directory := OBMetaNode named: 'directory'.
	file := OBMetaNode named: 'file'.
	directory childAt: #directories put: directory.
	directory childAt: #files put: file.
	^ directory