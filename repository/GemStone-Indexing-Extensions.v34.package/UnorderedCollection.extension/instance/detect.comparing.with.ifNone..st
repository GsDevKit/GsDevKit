*gemstone-indexing-extensions
detect: operandPath comparing: queryOp with: queryVal ifNone: exceptionBlock
    "operandPath - array of Symbols representing path to indexed variables used as operand 
     queryOp     - #'==', #'~~', #'=', #'~=', #<, #'<=', #>, #'>=' 
     queryVal    - operand 
    " 
	| result |
	(self _isValidQueryOperation: queryOp) 
		ifFalse: [ ^self error: 'Invalid query operator: ', queryOp printString ].
	result := self _detect: operandPath comparing: queryOp with: queryVal.
	result == #_incompletePathTraversal
		ifTrue: [ ^ exceptionBlock value ].
	^result