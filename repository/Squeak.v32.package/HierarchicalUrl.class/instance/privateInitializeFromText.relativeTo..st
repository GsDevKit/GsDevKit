parsing
privateInitializeFromText: aString relativeTo: aUrl 
	| remainder ind basePath |
	remainder _ aString.
	"set the scheme"
	schemeName _ aUrl schemeName.

	"a leading // means the authority is specified, meaning it is absolute"
	(remainder beginsWith: '//')
		ifTrue: [^ self privateInitializeFromText: aString].

	"otherwise, use the same authority"
	authority _ aUrl authority.
	port _ aUrl port.
	username _ aUrl username.
	password _ aUrl password.

	"get the query"
	ind _ remainder indexOf: $?.
	ind > 0
		ifTrue: [query _ remainder copyFrom: ind + 1 to: remainder size.
			remainder _ remainder copyFrom: 1 to: ind - 1].

	"get the path"
	(remainder beginsWith: '/')
		ifTrue: [ basePath := #() ]
		ifFalse: [ basePath := aUrl path ].
	path := self privateParsePath: remainder  relativeTo: basePath.