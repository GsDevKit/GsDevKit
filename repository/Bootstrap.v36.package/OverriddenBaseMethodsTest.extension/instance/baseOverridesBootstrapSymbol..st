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
	}