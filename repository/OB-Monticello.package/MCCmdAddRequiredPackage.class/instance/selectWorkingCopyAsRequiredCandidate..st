private
selectWorkingCopyAsRequiredCandidate: aWorkingCopy

	| wc package aPackage seen remaining packages |
	wc := target workingCopy.
	aPackage := aWorkingCopy package.
	(wc requiredPackages includes: aPackage)
		ifTrue: [ ^false ].
	package := wc package.
	(package = aPackage)
		ifTrue: [ ^false ].
	"check for requiredPackages cycles"
	seen := IdentitySet new.
	packages := aWorkingCopy requiredPackages.
	[ | newPackages |
		newPackages := IdentitySet new.
		packages do: [:pkg | 
			pkg = package ifTrue: [ ^false ].
			(seen includes: pkg)
				ifFalse: [ newPackages addAll: pkg workingCopy requiredPackages ].
			seen add: pkg ].
		packages := newPackages.
		packages isEmpty] untilTrue.
	^true