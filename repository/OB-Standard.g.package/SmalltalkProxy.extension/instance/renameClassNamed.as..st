*ob-standard
renameClassNamed: oldName as: newName
	| oldClass oldDefinition newClass |
	(oldClass := self at: oldName asSymbol ifAbsent: [nil]) == nil
		ifTrue: [ ^self error: 'Class ', oldName, ' does not exist' ].
	(oldClass environment hasClassNamed: newName) 
		ifTrue: [ ^ self error: newName , ' already exists' ].
	oldDefinition := oldClass 
						definitionWithClassName: newName 
						superClassName: (oldClass superClass == nil 
							ifTrue: ['nil'] ifFalse: [oldClass superClass name]).
	newClass := oldDefinition evaluate.
	newClass 
		copyMethodsFrom: oldClass 
		dictionaries: GsSession currentSession symbolList.
	newClass classComment: oldClass classComment.
	oldClass subclasses do: [:subClass | | def |
		"change the superclass for all subclasses"
		def := subClass 
				definitionWithClassName: subClass name 
				superClassName: newName.
		def evaluate].
	oldClass removeFromSystem ifFalse: [ ^ self error: oldClass name , ' not removed' ].