installing
installStream: aStream in: aSymDict
    GsPackagePolicy current homeSymbolDict: aSymDict.
	(self on: aStream) install