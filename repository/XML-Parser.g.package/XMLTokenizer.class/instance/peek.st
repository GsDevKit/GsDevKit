streaming
peek
	"Return the next character from the current input stream. If the current stream poop to next nesting level if there is one.
	Due to the potential nesting of original document, included documents and replacment texts the streams are held in a stack representing the nested streams. The current stream is the top one."
	peekChar
		ifNil: [
			nestedStreams ifNotNil: [self checkNestedStream].
			peekChar := stream atEnd 
				ifTrue: [ nil ]
				ifFalse: [ stream next].
			^peekChar ]
		ifNotNil: [^peekChar]