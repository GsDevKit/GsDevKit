tokenizing
nextEndTag
	| string |
	"Skip /"
	self next.
	string _ self nextTrimmedBlanksUpTo: $>.
	self handleEndTag: string