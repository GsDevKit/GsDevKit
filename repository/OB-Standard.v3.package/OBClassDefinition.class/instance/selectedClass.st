confirmation
selectedClass
	^ environment at: (self nameOfClassDefinedBy: template) ifAbsent: [nil]