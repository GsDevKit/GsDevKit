private
requestCategoryToUser
	"Present a choice of categories or prompt for a new category name."
	| labels reject lines answer newLabel |
	newLabel := 'new...'.
	labels := OrderedCollection with: newLabel.
	reject := Set new.
	reject
		addAll: target organization categories ;
		add: ClassOrganizer default.
	lines := OrderedCollection new.
	self 
		getCategories: labels
		reject: reject
		lines: lines.
	^ (labels size = 1 or: 
			[ answer := OBChoiceRequest 
				prompt: 'Add Category'
				labels: labels
				values: labels
				lines: lines.
			answer ifNil: [ ^ nil ].
			answer = newLabel ]) 
		ifTrue: [
			OBTextRequest 
				prompt: 'Please type new category name' 
				template: (self categoryTemplateFor: target organization) ]
		ifFalse: [ answer ]