*gemstone-indexing-extensions
low: lowOperand comparing: lowOperator detect: operandPath comparing: hiOperator high: hiOperand ifNone: exceptionBlock
    "lowOperand  - operand 
     lowOperator - #'<=' or  #< 
     operandPath - array of Symbols representing path to indexed variables used as operand 
     hiOperator  - #'<=' or  #< 
     hiOperand   - operand 
    " 
	| evaluator result |
	((self _isValidRangeOperation: lowOperator) and: [ self _isValidRangeOperation: hiOperator ])
		ifFalse: [ self error: 'Range operators must be either #< or #''<=''.' ].
	evaluator := self _evaluatorFor: operandPath isRangeEqualityOperation: false.
	result := evaluator 
		findFirstValueGreaterThan: lowOperand
		andEquals: lowOperator == #'<='
		andLessThan: hiOperand
		andEquals: hiOperator == #'<='.
	result == #_incompletePathTraversal
		ifTrue: [ ^ exceptionBlock value ].
	^result