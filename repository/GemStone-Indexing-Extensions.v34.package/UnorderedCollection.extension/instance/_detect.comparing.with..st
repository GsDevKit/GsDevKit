*gemstone-indexing-extensions
_detect: operandPath comparing: queryOp with: queryVal 
    "operandPath - array of Symbols representing path to indexed variables used as operand 
     queryOp     - #'==', #'~~', #'=', #'~=', #<, #'<=', #>, #'>=' 
     queryVal    - operand 
    " 
	| evaluator |
	evaluator := self _evaluatorFor: operandPath isRangeEqualityOperation: (self _isRangeEqualityOperation: queryOp).
	queryOp == #'=='
		ifTrue: [ ^evaluator findFirstValueIdenticalTo: queryVal ].
	queryOp == #'~~'
		ifTrue: [ ^evaluator findFirstValueNotIdenticalTo: queryVal ].
	queryOp == #'='
		ifTrue: [ ^evaluator findFirstValueEqualTo: queryVal ].
	queryOp == #'~='
		ifTrue: [ ^evaluator findFirstValueNotEqualTo: queryVal ].
	queryOp == #'<'
		ifTrue: [ ^evaluator findFirstValueLessThanKey: queryVal andEquals: false ].
	queryOp == #'<='
		ifTrue: [ ^evaluator findFirstValueLessThanKey: queryVal andEquals: false ].
	queryOp == #'>'
		ifTrue: [ ^evaluator findFirstValueGreaterThanKey: queryVal andEquals: false ].
	queryOp == #'>='
		ifTrue: [ ^evaluator findFirstValueGreaterThanKey: queryVal andEquals: true ].
