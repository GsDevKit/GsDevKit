*core-squeak
beginsWith: prefix
	"Answer whether the receiver begins with the given prefix string.
	The comparison is case-sensitive."
	"Not the same implementation as in Squeak"

	| stream |

	self size < prefix size ifTrue: [^ false].
	stream := self readStream.
	^prefix allSatisfy: [:each | each = stream next]