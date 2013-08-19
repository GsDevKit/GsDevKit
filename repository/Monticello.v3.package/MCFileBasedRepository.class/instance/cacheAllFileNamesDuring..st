private
cacheAllFileNamesDuring: aBlock
	allFileNames ifNotNil:[^aBlock value].
	allFileNames := self allFileNames.
	^ aBlock ensure: [allFileNames := nil]