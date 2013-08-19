actions
handlePackageRename: oldPackageName to: newPackageName 
	categories do:
		[ : each | (each beginsWith: newPackageName) ifFalse:
			[ | newCategoryName |
			newCategoryName := self
				newCategoryNameFor: each
				givenRenameFrom: oldPackageName
				to: newPackageName.
			(SystemOrganizer new categories includes: newCategoryName)
				ifTrue: [ SystemOrganizer new removeCategory: newCategoryName ].
			SystemOrganizer new 
				renameCategory: each
				toBe: newCategoryName ] ]