emphasis
makeSelectorBold
	"For formatting Smalltalk source code, set the emphasis of that portion of 
	the receiver's string that parses as a message selector to be bold."

	| selector i |
	string size = 0 ifTrue: [^ self].
	i _ 0.
	[(string at: (i _ i + 1)) isSeparator] whileTrue.
	(string at: i) = $[ ifTrue: [^ self].  "block, no selector"
	[ selector := ChangeList parseSelector: string] on: Error do: [^ self].
	self makeBoldFrom: 1 to: (selector size min: string size)