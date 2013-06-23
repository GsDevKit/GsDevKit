*gemstone-indexing-extensions
detect: operandPath comparing: queryOp with: queryVal
    "operandPath - array of Symbols representing path to indexed variables used as operand 
     queryOp     - #'==', #'~~', #'=', #'~=', #<, #'<=', #>, #'>=' 
     queryVal    - operand 
    " 
	^self detect: operandPath comparing: queryOp with: queryVal ifNone: [ ^ self error: 'No elements detected' ]
