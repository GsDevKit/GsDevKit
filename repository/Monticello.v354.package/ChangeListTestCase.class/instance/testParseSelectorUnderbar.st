tests
testParseSelectorUnderbar

	| x | 
	self assert: (x := ChangeList  parseSelector: '_classForRubyClassVar
  ^ self virtualClass _classForRubyClassVar

') = '_classForRubyClassVar'.

