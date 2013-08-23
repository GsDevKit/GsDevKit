*ob-standard
definitionWithClassName: className superClassName: superClassName

	^ self _ogDefinitionInContext: System myUserProfile 
		className: className 
		superClassName: superClassName