tests
testParseSelectorCases
	| ar x | 
	ar := #(selectorCase1 'selector' selectorCase2 'selector:' selectorCase3 'selector:' selectorCase4 'selector:selector:selector:selector:' selectorCase5 'selector' selectorCase6 'selector').
	1 to: ar size by: 2 do: [:index |
		self assert: (x := ChangeList parseSelector: (self perform: (ar at: index))) = (ar at: index + 1) ].