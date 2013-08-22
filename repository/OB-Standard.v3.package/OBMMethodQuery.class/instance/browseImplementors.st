private
browseImplementors
	| node |
	node := OBSelectorNode on: pattern asSymbol.
	(browser announce: (OBShowImplementors of: node)) execute