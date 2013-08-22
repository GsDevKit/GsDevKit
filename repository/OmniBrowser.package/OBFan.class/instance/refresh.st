accessing
refresh
	| oldChildren |
	oldChildren := children.
	children := parent defaultChildNodes.
	^ children ~= oldChildren