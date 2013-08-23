accessing
node
	^ column 
		ifNil: [node]
		ifNotNil: [column selectedNode]