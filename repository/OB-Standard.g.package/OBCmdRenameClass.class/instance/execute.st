execution
execute
	| newName |
	newName := OBTextRequest
					prompt: 'Please type new class name' 
					template: target theNonMetaClassName asString.
	newName ifNotNil:	[ | oldName binding |
		oldName := target theNonMetaClass name.
		binding := target theNonMetaClass environment associationAt: oldName.
		Smalltalk renameClassNamed: oldName as: newName.
		self doAutoCommit.
		(SystemNavigation default allCallsOn: binding) isEmpty 
			ifFalse: [OBReferencesBrowser
						browseRoot: target
						title: 'Obsolete references to']].
	