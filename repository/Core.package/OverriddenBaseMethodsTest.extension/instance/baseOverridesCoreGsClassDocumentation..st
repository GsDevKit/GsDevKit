*core
baseOverridesCoreGsClassDocumentation: known

	self 
		addTo: known 
		forBehavior: GsClassDocumentation 
		overrides: {
			#itsClass: -> 'itsClass: aClass

itsClass := aClass .
^ self
'.
	}