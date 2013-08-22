class definition
defineClass: definition notifying: aController
	| newClass |
	(self confirmDefinition: definition) ifFalse: [^ false].
	newClass := definition evaluate.
	newClass ifNil: [^ false].
	self signalSelectionOf: newClass.
	^ true