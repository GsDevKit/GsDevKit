private
categoryTemplateFor: anOrganization
	^ anOrganization isClassOrganizer
		ifTrue: ['category name']
		ifFalse: ['Category-Name']