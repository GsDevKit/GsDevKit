accessing
pathParts
	"Return the path from the root of the file system to this directory as an array of directory names."

	^ (pathName findTokens: self pathNameDelimiter asString) asArray