*gemstone-indexing-extensions
select: operandPath comparing: queryOp with: queryVal 
    "operandPath - array of Symbols representing path to indexed variables used as operand 
     queryOp     - #'==', #'~~', #'=', #'~=', #<, #'<=', #>, #'>=' 
     queryVal    - operand 
    " 
	| result |
	(self _isValidQueryOperation: queryOp) 
		ifFalse: [ ^self error: 'Invalid query operator: ', queryOp printString ].
	result := self _select: operandPath comparing: queryOp with: queryVal.
	
	result class = self speciesForSelect
    		ifFalse: [ | tmpResult |
        		tmpResult := self speciesForSelect new.
        		tmpResult addAll: result.
        		result := tmpResult].                                                                           
	result == self ifTrue: [ ^ result copy ].                                                                                             
	^ result
