*bootstrap
baseOverridesBootstrapSymbol: known
	self
		addTo: known
		forBehavior: Symbol
		overrides: {
			#'->' -> '-> anObject

"Returns a SymbolAssociation with the receiver as the key and the given object
 as the value."

^ SymbolAssociation newWithKey: self value: anObject
'.
			#'precedence' -> 'precedence

"Returns the precedence of the receiver, were it a message selector, with
 1=unary, 2=binary and 3=keyword."

^self isInfix
    ifTrue: [ 2 ]
    ifFalse: [ self isKeyword ifTrue: [ 3 ]
                              ifFalse: [ 1 ] ]
'.
	}