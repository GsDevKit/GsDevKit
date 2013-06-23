private
findOverridenMethodsFor: behavior overrides: overrides knownOverrides: knownOverrides

	| hasSessionMethods rawMethodDict sessionMethodDict |
	hasSessionMethods := false.
	GsPackagePolicy current packagesDo: [:package | 
		(package sessionMethods at: behavior ifAbsent: []) ~~ nil
			ifTrue: [ hasSessionMethods := true ]].
	rawMethodDict := behavior _rawMethodDict.
	sessionMethodDict := behavior _methodDict.
	sessionMethodDict keysAndValuesDo: [:selector :sessionMethod | | baseMethod |
		baseMethod := rawMethodDict at: selector ifAbsent: [].
		(baseMethod ~~ nil and: [ baseMethod ~~ sessionMethod ])
			ifTrue: [ | set known knownSourceSet|
				known := knownOverrides at: behavior ifAbsent: [].
				knownSourceSet := known == nil 
					ifTrue: [ #() ]
					ifFalse: [ known at: selector ifAbsent: [ #() ]].
				(knownSourceSet includes: baseMethod sourceString) not
						ifTrue: [ 
						set := overrides 
							at: behavior
							ifAbsent: [
								set := IdentitySet new.
								overrides at: behavior put: set.
								set ].
						set add: { baseMethod. sessionMethod. knownSourceSet.} ]]]