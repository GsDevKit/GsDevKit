private
getCompiledMethod
	| def sel cls |
	def := requestor getDefinition.
	def theClass == nil ifTrue: [ ^nil ]. "anonymous method - a doIt"
	sel := def selector.
	cls := def selectedClass.
	sel == nil ifTrue: [ ^nil ].
	cls := cls whichClassIncludesSelector: sel.
	^cls compiledMethodAt: sel.
