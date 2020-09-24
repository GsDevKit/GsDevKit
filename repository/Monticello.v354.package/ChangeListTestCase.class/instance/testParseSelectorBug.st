tests
testParseSelectorBug

	| x | 
	self assert: (x := ChangeList parseSelector: 'appendStatement: aNode
  self add: aNode') = 'appendStatement:'.

