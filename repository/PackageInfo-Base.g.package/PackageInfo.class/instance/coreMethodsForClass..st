testing
coreMethodsForClass: aClass
	^ (aClass selectors difference:
		((self foreignExtensionMethodsForClass: aClass) collect: [:r | r methodSymbol]))
			asArray collect: [:sel | self referenceForMethod: sel ofClass: aClass]