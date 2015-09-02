execution
execute
	| expression value |
	expression := OBTextRequest
		prompt: 'Enter expression for return value:' 
		template: 'nil'.
	value:= expression
		evaluateInContext: target context receiver 
		symbolList: GsSession currentSession symbolList.
	target process popTo: target context homeContext value: value.
	self update