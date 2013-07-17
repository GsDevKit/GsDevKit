dependencies
externalSubclasses
	| pkgClasses subClasses |
	pkgClasses _ self classes.
	subClasses _ Set new.
	pkgClasses do: [:c | subClasses addAll: (c allSubclasses)].
	^ subClasses difference: pkgClasses