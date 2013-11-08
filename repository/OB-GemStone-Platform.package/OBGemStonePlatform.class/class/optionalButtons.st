preferences
optionalButtons

	^self preferencesDictionary at: #optionalButtons ifAbsent: [ true ]
