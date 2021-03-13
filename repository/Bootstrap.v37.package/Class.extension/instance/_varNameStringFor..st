*bootstrap
_varNameStringFor: varNames
	"Returns a string variable names."

	| result |
	result := String new: 0.
	varNames do: [:each |
   		result addLast: $  .
  		(each includesValue: $') 
    			ifTrue:[ result addAll: each _asSource ]
    			ifFalse:[ result addAll: each ]].
	^result