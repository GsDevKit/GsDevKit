instance creation
on: pathString
	"Return a new file directory for the given path, of the appropriate FileDirectory subclass for the current OS platform."

	| pathName |
	"If path ends with a delimiter (: or /) then remove it"
	(((pathName := pathString) endsWith: self pathNameDelimiter asString) and: [pathString size > 1]) ifTrue: [
		pathName := pathName copyFrom: 1 to: pathName size - 1].
	^ self new pathString: pathName