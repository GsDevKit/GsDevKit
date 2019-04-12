private
findOverridenMethodsFor: behavior overrides: overrides knownOverrides: knownOverrides

	| hasSessionMethods persistentMethodDict sessionMethodDict |
	hasSessionMethods := false.
	GsPackagePolicy current packages_Do: [:package | 
		(package sessionMethods at: behavior ifAbsent: []) ~~ nil
			ifTrue: [ hasSessionMethods := true ]].
	persistentMethodDict := behavior persistentMethodDictForEnv: 0.
	sessionMethodDict := behavior methodDictForEnv: 0.
	sessionMethodDict keysAndValuesDo: [:selector :sessionMethod | | baseMethod |
		baseMethod := persistentMethodDict at: selector ifAbsent: [].
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