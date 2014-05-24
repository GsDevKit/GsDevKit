accessing
peek
"Returns the next element in the collection, but does not alter the current
 position reference.  If the receiver is at the end of the collection, returns
 nil."
	| pos result |

	pos := self position.
	result := self next.
	self position: pos.
	^result