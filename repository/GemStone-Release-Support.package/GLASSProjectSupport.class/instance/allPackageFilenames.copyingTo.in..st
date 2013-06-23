bootstrap packages
allPackageFilenames: aVersionString copyingTo: repository in: dir
	| project version metacelloVersionString repositoryMap directives packageDirectives projectPackageDirective |
	repositoryMap := Dictionary new.
	project := self configuration project.
	version := project version: aVersionString.
	version ignoreImage: true.	
	packageDirectives := OrderedCollection new.
	(self configurationDirectiveFor: project) packageDirectivesDo: [:pkgDirective |  packageDirectives add:  pkgDirective ].
	directives := (version fetch: 'ALL') loadDirective.
	directives packageDirectivesDo: [:pkgDirective | packageDirectives add:  pkgDirective ].
	(self filterPackageSpecs: packageDirectives) do: [:directive | |  filename |
		filename := directive file.
		(dir fileExists: filename, '.mcz')
			ifTrue: [ Transcript cr; show: 'Up to date: ', filename printString ]
			ifFalse: [ 
				Transcript cr; show: 'Copying: ', filename printString.
				repository storeVersion: directive externalReference version].
		repositoryMap at: directive put: { directive repository. } ].
	^repositoryMap
