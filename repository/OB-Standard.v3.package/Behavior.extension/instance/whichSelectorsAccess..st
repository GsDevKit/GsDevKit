*ob-standard-accessing
whichSelectorsAccess: instVarName 
	"Answer a set of selectors whose methods access the argument, 
	instVarName, as a named instance variable."

	| ivName |
	ivName := instVarName asSymbol.
	^self selectors select: [:sel | | meth |
		meth := self compiledMethodAt: sel.
		(meth instVarsAccessed includes: ivName)
	].