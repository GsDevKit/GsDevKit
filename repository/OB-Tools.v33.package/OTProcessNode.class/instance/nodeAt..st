navigation
nodeAt: aContext
	"Answer a cached node of the receiving process."

	^ cache at: aContext ifAbsentPut: [ OBPlatform current otContextNodeClass on: process context: aContext ]