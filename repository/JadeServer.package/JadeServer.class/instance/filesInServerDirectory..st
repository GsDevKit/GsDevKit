repository
filesInServerDirectory: aPath

	^self stringFromCollection: (ServerFileDirectory on: aPath) fileNames