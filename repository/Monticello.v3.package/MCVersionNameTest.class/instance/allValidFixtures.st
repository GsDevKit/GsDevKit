as yet unclassified
allValidFixtures
	^ Array streamContents: [:stream |
		stream nextPutAll: self standardFixtures.
		self diffyFixtures pairsDo: [:diffy :base | stream nextPut: diffy]].