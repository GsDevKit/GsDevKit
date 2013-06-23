install script
metacelloPackageVersionsFor: aVersionString
	"a fair amount of cheating going on here ... the correct algorithm is used in 3.0 (we just load configurations).
	 This mess used only for 2.4.5 and subsequent 2.x builds"

	| default version excluded packagesInLoadOrder |
	version := ConfigurationOfMetacello project version: aVersionString.
	excluded := version projects collect: [:project | project name ].
	excluded add: 'OB-Metacello'. "exclude projects and OB-Metacello"
	default := (version packagesForSpecNamed: 'default') collect: [:pkg | pkg name ].
	packagesInLoadOrder := OrderedCollection with: 'Gofer-Core.gemstone-dkh.135'. "Hard-wired package for Gofer"
	default := version defaultPackageNamesToLoad: default.
	version spec packageSpecsInLoadOrder do:[:spec | 
		(excluded includes: spec name)
			ifFalse: [
				(default includes: spec name)
					ifTrue: [ packagesInLoadOrder add: spec file ]]].
	^packagesInLoadOrder