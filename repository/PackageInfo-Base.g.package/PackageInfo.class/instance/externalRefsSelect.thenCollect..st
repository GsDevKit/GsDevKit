dependencies
externalRefsSelect: selBlock thenCollect: colBlock
	| pkgMethods dependents refs extMethods otherClasses otherMethods classNames |

	classNames _ self classes collect: [:c | c name].
	extMethods _ self extensionMethods collect: [:mr | mr methodSymbol].
	otherClasses _ self externalClasses difference: self externalSubclasses.
	otherMethods _  otherClasses gather: [:c | c selectors].
	pkgMethods _ self methods asSet collect: [:mr | mr methodSymbol].
	pkgMethods removeAllFoundIn: otherMethods.

	dependents _ Set new.
	otherClasses do: [:c |
		c selectorsAndMethodsDo:
			[:sel :compiled |
			(extMethods includes: sel) ifFalse: 
				[refs _ compiled literals select: selBlock thenCollect: colBlock.
				refs do: [:ea |
					((classNames includes: ea) or: [pkgMethods includes: ea])
							ifTrue: [dependents add: (self referenceForMethod: sel ofClass: c) -> ea]]]]].
	^ dependents