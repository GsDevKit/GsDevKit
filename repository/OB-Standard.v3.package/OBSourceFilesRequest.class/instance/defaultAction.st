exceptionDescription
defaultAction
	^ Smalltalk at: #SourceFiles ifAbsent: [ self error: 'No SourceFiles available' ]