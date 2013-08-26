tests
verifyRenameTo: newName 
	
	| oldWorkingCopy oldDefinitions newWorkingCopy |
	oldWorkingCopy := workingCopy.
	oldDefinitions := workingCopy package snapshot definitions.
	[ 
		newWorkingCopy := workingCopy renameToBe: newName.
		self
			 assert: oldWorkingCopy ancestors = newWorkingCopy ancestors ;
			 assert: newWorkingCopy needsSaving ;
			 assert: newWorkingCopy requiredPackages = oldWorkingCopy requiredPackages ;
			 assert: newWorkingCopy repositoryGroup repositories = oldWorkingCopy repositoryGroup repositories ;
			 assert: newWorkingCopy package snapshot definitions size = oldDefinitions size ;
			 assert: (MCWorkingCopy registry includesKey: newWorkingCopy package) ;
			 assert: (MCWorkingCopy registry includes: newWorkingCopy) ;
			 deny: (MCWorkingCopy registry includes: oldWorkingCopy).
		workingCopy := newWorkingCopy ] ensure: [ | organizer |
			organizer := SystemOrganizer new.
			" ensure that we rename the category back to the original name "
			(organizer categories includes: newName) ifTrue: [
				organizer
					renameCategory: newName
					toBe: oldWorkingCopy packageName ] ]