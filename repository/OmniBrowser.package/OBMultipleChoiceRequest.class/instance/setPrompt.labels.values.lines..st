initializing
setPrompt: aString labels: labelArray values: valueArray lines: lineArray
	super setPrompt: aString labels: labelArray values: valueArray lines: lineArray.
	selection := IdentitySet withAll: valueArray