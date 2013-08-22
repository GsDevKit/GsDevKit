actions
update
	| processNode |
	processNode := requestor browser root.
	processNode targetContext: nil.
	self announce: (OBChildrenChanged node: processNode).
	self announce: (OBSelectingNode node: processNode contextNode)