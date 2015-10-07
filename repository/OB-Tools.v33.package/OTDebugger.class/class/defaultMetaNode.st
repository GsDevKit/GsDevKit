configuration
defaultMetaNode
	| process context |
	process := OBMetaNode named: 'process'.
	context := OBMetaNode named: 'context'.
	context addFilter: OTBreakpointFilter new.
	process childAt: #longStack put: context.
	^ process