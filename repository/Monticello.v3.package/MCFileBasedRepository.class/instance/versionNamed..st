versions
versionNamed: aMCVersionName
	"For FileBased repositories, aMCVersionName must have the appropriate extension!  :-("
	| v |
	v := self cache at: aMCVersionName ifAbsent: [self loadVersionFromFileNamed: aMCVersionName].
	self resizeCache: cache.
	(v notNil and: [v isCacheable]) ifTrue: [cache at: aMCVersionName asMCVersionName put: v].
	^ v