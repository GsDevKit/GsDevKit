streaming
nextUpToAll: delimitingString
	| string |
	"self unpeek."	"#unpeek does not work in GemStone, so just add the character to the result."
	string := (peekChar notNil ifTrue: [peekChar asString] ifFalse: ['']) , (self stream upToAll: delimitingString).
	peekChar := nil.
	"The following not needed for GemStone"
	"self stream skip: delimitingString size negated."
	(self stream next: delimitingString size) = delimitingString
		ifFalse: [self parseError: 'XML no delimiting ' , delimitingString printString , ' found'].
	^string