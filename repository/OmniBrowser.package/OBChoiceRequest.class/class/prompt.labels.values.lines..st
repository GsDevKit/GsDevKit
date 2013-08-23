exceptionInstantiator
prompt: aString labels: labelArray values: valueArray lines: lineArray
	^ (self new 
		setPrompt: aString 
		labels: labelArray 
		values: valueArray 
		lines: lineArray) 
			signal