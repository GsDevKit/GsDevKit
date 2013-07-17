testing
isOverrideOfYourMethod: aMethodReference
	^ (self isYourClassExtension: aMethodReference category) not and:
		[(self changeRecordForOverriddenMethod: aMethodReference) notNil]