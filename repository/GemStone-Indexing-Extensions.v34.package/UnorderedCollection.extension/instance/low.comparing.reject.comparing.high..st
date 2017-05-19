*gemstone-indexing-extensions
low: lowOperand comparing: lowOperator reject: operandPath comparing: hiOperator high: hiOperand 
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
		findAllValuesGreaterThan: lowOperand
		andEquals: lowOperator == #'<='
		andLessThan: hiOperand
		andEquals: hiOperator == #'<='.
	result := self _asIdentityBag - result _asIdentityBag.
	result class = self speciesForSelect
    		ifFalse: [ | tmpResult |
        		tmpResult := self speciesForSelect new.
        		tmpResult addAll: result.
        		result := tmpResult].                                                                           
	^ result
