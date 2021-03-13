public
accept: aText notifying: aController

	| processNode |
	super accept: aText notifying: aController.
	processNode := aController browser root.
	aController announce: (OBChildrenChanged node: processNode).
	aController announce: (OBSelectingNode node: processNode contextNode)