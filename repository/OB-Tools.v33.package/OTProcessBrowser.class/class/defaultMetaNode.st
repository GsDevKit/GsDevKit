configuration
defaultMetaNode
	| scheduler process context |
	scheduler := OBMetaNode named: 'scheduler'.
	process := OBMetaNode named: 'process'.
	context := OBMetaNode named: 'context'.
	scheduler childAt: #processes put: process.
	process childAt: #shortStack put: context.
	^ scheduler