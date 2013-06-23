*core-bootstrap
_description: aDescription

	"Update the description of this Class.  Returns the argument."

	self commentStamp: self changeStamp.
	^ description := aDescription
