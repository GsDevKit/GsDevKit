as yet unclassified
list
	^ filter 
		ifNotNil: [filter listForNode: column selectedNode]
		ifNil: [#()]