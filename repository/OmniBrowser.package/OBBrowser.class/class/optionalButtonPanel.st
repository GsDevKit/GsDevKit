configuration
optionalButtonPanel
	^ OBPlatform current optionalButtons ifTrue: [OBFixedButtonPanel new]