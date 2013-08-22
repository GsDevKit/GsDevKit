navigating
accessors
	| accessors |
	accessors := OrderedCollection new.
	self theClass withAllSubAndSuperclassesDo: [:class |
		(class whichSelectorsAccess: name) asSortedCollection
			do: [:ea | ea = #DoIt ifFalse: [accessors add: 
					(self referenceForMethod: ea ofClass: class name)]]].
	^ accessors asArray	collect: [:ref | 
				(OBMethodNode on: ref)
					symbolDictionary: self symbolDictionary;
					yourself ]