parsing
privateInitializeFromText: aString 
	| remainder ind specifiedSchemeName |
	remainder _ aString.
	schemeName ifNil: 
			[specifiedSchemeName _ Url schemeNameForString: remainder.
			specifiedSchemeName ifNotNil: 
					[schemeName _ specifiedSchemeName.
					remainder _ remainder copyFrom: schemeName size + 2 to: remainder size].
			schemeName ifNil: 
					["assume HTTP"

					schemeName _ 'http']].

	"remove leading // if it's there"
	(remainder beginsWith: '//') 
		ifTrue: [remainder _ remainder copyFrom: 3 to: remainder size].


	"get the query"
	ind _ remainder indexOf: $?.
	ind > 0 
		ifTrue: 
			[query _ remainder copyFrom: ind + 1 to: remainder size.
			remainder _ remainder copyFrom: 1 to: ind - 1].

	"get the authority"
	ind _ remainder indexOf: $/.
	ind > 0 
		ifTrue: 
			[ind = 1 
				ifTrue: [authority _ '']
				ifFalse: 
					[authority _ remainder copyFrom: 1 to: ind - 1.
					remainder _ remainder copyFrom: ind + 1 to: remainder size]]
		ifFalse: 
			[authority _ remainder.
			remainder _ ''].

	"extract the username+password"
	(authority includes: $@) 
		ifTrue: 
			[username _ authority copyUpTo: $@.
			authority _ authority copyFrom: (authority indexOf: $@) + 1
						to: authority size.
			(username includes: $:) 
				ifTrue: 
					[password _ username copyFrom: (username indexOf: $:) + 1 to: username size.
					username _ username copyUpTo: $:]].

	"Extract the port"
	(authority includes: $:) 
		ifTrue: 
			[| lastColonIndex portString |
			lastColonIndex _ authority findLast: [:c | c = $:].
			portString _ authority copyFrom: lastColonIndex + 1 to: authority size.
			portString isAllDigits 
				ifTrue: 
					[port _ Integer readFromString: portString.
					(port > 65535) ifTrue: [self error: 'Invalid port number'].
					 authority _ authority copyFrom: 1 to: lastColonIndex - 1]
				ifFalse:[self error: 'Invalid port number']].

	"get the path"
	path _ self privateParsePath: remainder relativeTo: #() .