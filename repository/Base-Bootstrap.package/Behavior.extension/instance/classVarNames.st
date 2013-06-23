*base-bootstrap
classVarNames
	"Returns an Array of Symbols naming the class variables defined by this class.
	 Inherited class variables are not included; contrast with allClassVarNames."

	| keys |
	(classVars == nil)
		ifTrue: [ ^ Array new ].
	keys := classVars keys select: [:key | | val |
		val := classVars at: key.
		val == classVars
			ifTrue: [ 
				"filter out name of pool"
				(self inheritsFrom: SharedPool) not ]
			ifFalse: [ true ]].
   	^keys asArray
