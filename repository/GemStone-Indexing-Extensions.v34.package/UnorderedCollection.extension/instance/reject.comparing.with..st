*gemstone-indexing-extensions
reject: operandPath comparing: queryOp with: anOperand 
    "operandPath - array of Symbols representing path to indexed variables used as operand 
     queryOp      - #'==', #'~~', #'=', #'~=', #<, #'<=', #>, #'>=' 
     anOperand   - operand 
    " 
	| result tmpResult |
	(self _isValidQueryOperation: queryOp) 
		ifFalse: [ ^self error: 'Invalid query operator: ', queryOp printString ].
	result := self _asIdentityBag -
  		(self _select: operandPath comparing: queryOp with: anOperand ) _asIdentityBag.
	result class == self speciesForSelect
  		ifFalse: [
    			tmpResult := self speciesForSelect new.
    			tmpResult addAll: result.
    			result := tmpResult ].
	^ result
