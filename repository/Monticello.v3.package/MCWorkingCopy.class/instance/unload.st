operations
unload
	MCPackageLoader unloadPackage: self package.
	self unregisterSubpackages.
	self unregister.
	"make sure that the sessionMethodDictionary is updated correctly - bug38053"
	GsPackagePolicy current refreshSessionMethodDictionary