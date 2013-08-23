actions
moveToPackage
	| packagesNames selection packages |
	packages := PackageOrganizer default packages 
					asSortedCollection: [:a :b | a packageName <= b packageName].
	packagesNames := packages collect: [:ea | ea packageName].
	selection := OBChoiceRequest prompt: nil labels: packagesNames values: packages.
	selection ifNotNil: [selection addMethod: self reference].