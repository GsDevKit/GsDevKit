class initialization
initialize
	"XMLTokenizer initialize"

	| nameDelimiters |

	CharEscapes _ #( $& $" $' $> $< ) asSet.

	SeparatorTable  _ Array new: 256.
	SeparatorTable atAllPut: true.
	#(9 10 12 13 32) do: [:each | SeparatorTable at: each+1 put: false].

	LiteralChars _ Array new: 256.
	LiteralChars atAllPut: false.
	':-_.' do: [:each | LiteralChars at: each asciiValue put: true].
	1 to: 256 do: [:i | ((i-1) asCharacter isDigit or: [(i-1) asCharacter isLetter])
		ifTrue: [LiteralChars at: i put: true]].

	nameDelimiters _ #(9 10 12 13 32 61 "$= asInteger 61" 62 "$> asInteger" 47 "$/ asInteger").
	NameDelimiters _ Array new: 256.
	NameDelimiters atAllPut: false.
	nameDelimiters do: [:each | NameDelimiters at: each put: true].

	DigitTable _ Array new: 256.
	DigitTable atAllPut: -1.
	($0 to: $9) do: [:each | DigitTable at: each asciiValue put: each digitValue].
	($A to: $F) do: [:each | DigitTable at: each asciiValue put: each digitValue].