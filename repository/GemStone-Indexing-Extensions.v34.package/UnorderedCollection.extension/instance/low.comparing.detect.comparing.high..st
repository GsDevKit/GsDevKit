*gemstone-indexing-extensions
low: lowOperand comparing: lowOperator detect: operandPath comparing: hiOperator high: hiOperand 
    "lowOperand  - operand 
     lowOperator - #'<=' or  #< 
     operandPath - array of Symbols representing path to indexed variables used as operand 
     hiOperator  - #'<=' or  #< 
     hiOperand   - operand 
    " 
	^self 
		low: lowOperand 
		comparing: lowOperator 
		detect: operandPath 
		comparing: hiOperator 
		high: hiOperand 
		ifNone: [ ^ self error: 'No elements detected' ]
