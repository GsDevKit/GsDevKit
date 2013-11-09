private
find
	((isWild not and: [ pattern size = 1 and: [Smalltalk hasClassNamed: pattern first asSymbol]]) or: [self searchNode classes size = 1])
		ifTrue: [self openClassBrowser]
		ifFalse: [self openSearchBrowser]
