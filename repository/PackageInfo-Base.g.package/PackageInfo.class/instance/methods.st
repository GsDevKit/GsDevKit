listing
methods
	^ (self extensionMethods, self coreMethods)
		select: [:method | method isValid and: [(#(DoIt DoItIn:) includes: method methodSymbol) not]]