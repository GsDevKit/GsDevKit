accessing
nameComponents
	| nm ar |
	nm := (filename copyUpToLast: $.) copyUpTo: $(.
	ar := nm last isDigit ifFalse: [Array with: nm with: '' with: 0 with: filename with: self]
		ifTrue: [ | vrsn str |
			str := ((nm copyAfterLast: $-) copyAfterLast: $.).
			vrsn := str isEmpty
				ifTrue: [0]
				ifFalse: [[ str asInteger ] on: Error do: [:ex | 0 ]].
			Array
				with: (nm copyUpToLast:  $-)		"pkg name"
				with: ((nm copyAfterLast: $-) copyUpTo: $.)				"user"
				with:  vrsn "version"
				with: filename
				with: self ].
	^ar