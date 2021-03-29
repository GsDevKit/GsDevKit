operations
fullPathFor: path
	"Return the fully-qualified path name for the given file."

	path isEmpty ifTrue: [^ pathName].
	path first = $/ ifTrue: [^ path].
	^ pathName = '/'			"Only root dir ends with a slash"
		ifTrue: ['/' , path]
		ifFalse: [pathName , '/' , path]