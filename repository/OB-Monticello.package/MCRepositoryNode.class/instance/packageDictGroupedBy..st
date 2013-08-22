navigating
packageDictGroupedBy: aSelector
	| versionNodes packageDict n |
	versionNodes _ self repository versionNodes.
	packageDict _ Dictionary new.
	versionNodes do:
		[:ea |
		n _ ea perform: aSelector.
		n == nil ifTrue: [ n := '--not loaded--' ].
		(packageDict at: n ifAbsentPut:
			[(MCPackageNode name: n repository: repository)
				packageName: ea packageName;
				yourself])
				addVersion: ea].
	^ packageDict