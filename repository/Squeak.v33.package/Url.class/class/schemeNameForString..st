parsing
schemeNameForString: aString
	"get the scheme name from a string, or return nil if it's not specified.  used in internal parsing routines--an outsider may as well use asUrl.  lowercases the return value."
	"Url schemeNameForString: 'http://www.yahoo.com'"
	"Url schemeNameForString: '/etc/passwed'"
	"Url schemeNameForString: '/etc/testing:1.2.3'"

	| idx schemeName |
	idx _ aString indexOf: $: ifAbsent: [^ nil].
	schemeName _ aString copyFrom: 1 to: idx - 1.
	(schemeName allSatisfy: [:each | each isLetter]) ifFalse: [^ nil].
	^ schemeName asLowercase