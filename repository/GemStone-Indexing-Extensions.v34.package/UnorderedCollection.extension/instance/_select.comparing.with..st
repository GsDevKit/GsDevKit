*gemstone-indexing-extensions
_select: operandPath comparing: queryOp with: queryVal 
    "operandPath - array of Symbols representing path to indexed variables used as operand 
     queryOp     - #'==', #'~~', #'=', #'~=', #<, #'<=', #>, #'>=' 
     queryVal    - operand 
    " 
	| evaluator |
	evaluator := self _evaluatorFor: operandPath isRangeEqualityOperation: (self _isRangeEqualityOperation: queryOp).
	queryOp == #'=='
		ifTrue: [ ^evaluator findAllValuesIdenticalTo: queryVal ].
	queryOp == #'~~'
		ifTrue: [ ^evaluator findAllValuesNotIdenticalTo: queryVal ].
	queryOp == #'='
		ifTrue: [ ^evaluator findAllValuesEqualTo: queryVal ].
	queryOp == #'~='
		ifTrue: [ ^evaluator findAllValuesNotEqualTo: queryVal ].
	queryOp == #'<'
		ifTrue: [ ^evaluator findAllValuesLessThanKey: queryVal andEquals: false ].
	queryOp == #'<='
		ifTrue: [ ^evaluator findAllValuesLessThanKey: queryVal andEquals: true ].
	queryOp == #'>'
		ifTrue: [ ^evaluator findAllValuesGreaterThanKey: queryVal andEquals: false ].
	queryOp == #'>='
		ifTrue: [ ^evaluator findAllValuesGreaterThanKey: queryVal andEquals: true ].
