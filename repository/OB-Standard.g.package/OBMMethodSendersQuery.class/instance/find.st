executing
find
	| node |
	node := OBSelectorNode on: pattern asSymbol.
	(browser announce: (OBShowSenders of: node)) execute