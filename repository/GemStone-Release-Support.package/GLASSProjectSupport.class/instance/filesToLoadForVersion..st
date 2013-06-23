install script
filesToLoadForVersion: version

	|  loadableNames files |
	loadableNames := (version resolveToLoadableSpecs: ((version packagesForSpecNamed: 'default') collect: [:each | each name ] )) collect: [:each | each name ].
	files  := OrderedCollection new.
	version packages do:[:each |  (loadableNames includes: each name) ifTrue: [ files add: each file ]].
	^files