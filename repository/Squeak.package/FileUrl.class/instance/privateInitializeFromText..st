private-initialization
privateInitializeFromText: aString
	"Calculate host and path from a file URL in String format.
	Some malformed formats are allowed and interpreted by guessing."

	| schemeName pathString bare hasDriveLetter stream char i |
	bare _ aString withBlanksTrimmed.
	schemeName _ Url schemeNameForString: bare.
	(schemeName isNil or: [schemeName ~= self schemeName])
		ifTrue: [
			host _ ''.
			pathString _ bare]
		ifFalse: [
			"First remove schemeName and colon"
			bare _ bare copyFrom: (schemeName size + 2) to: bare size.
			"A proper file URL then has two slashes before host,
			A malformed URL is interpreted as using syntax file:<path>."
			(bare beginsWith: '//')
				ifTrue: [i _ bare indexOf: $/ startingAt: 3.
						i=0 ifTrue: [
								host _ bare copyFrom: 3 to: bare size.
								pathString _ '']
							ifFalse: [
								host _ bare copyFrom: 3 to: i-1.
								pathString _ bare copyFrom: host size + 3 to: bare size]]
				ifFalse: [host _ ''.
						pathString _ bare]].
	self initializeFromPathString: pathString