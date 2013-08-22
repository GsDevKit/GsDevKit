accessing
latest

	^self class 
		on: (MethodVersionHistory uniqueInstance 
				versionsOfMethod: selector 
				in: classRef theClass) first