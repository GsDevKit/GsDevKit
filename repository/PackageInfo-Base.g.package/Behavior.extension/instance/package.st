*packageinfo-base
package
	| possiblePkgName package |
	possiblePkgName := self category copyUpTo: $-.
    	package := PackageOrganizer default packageNamed: possiblePkgName ifAbsent: [nil].
   	package ifNotNil: [(package classes includes: self) ifTrue: [^package]].
   	^PackageInfo allPackages detect: [:pkg | self category asString asLowercase beginsWith: pkg packageName asLowercase] ifNone: [nil]