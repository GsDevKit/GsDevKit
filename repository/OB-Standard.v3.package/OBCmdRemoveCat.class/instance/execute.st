execution
execute
	| organization list choice |
	organization := target container organization.
	organization isClassOrganizer
		ifTrue: [ list := organization selectorsIn: target name ]
		ifFalse: [ list := organization listAtCategoryNamed: target name ].
	list isEmpty 
		ifTrue: [ | res |
			res := target remove.
			self doAutoCommit.
			^res].
	choice := OBConfirmationRequest prompt: 'Are you sure you want to
remove this category 
and all its elements?' confirm: 'Remove'.
	choice 
		ifTrue: [ | res |
			res := target remove.
			self doAutoCommit.
			^res]