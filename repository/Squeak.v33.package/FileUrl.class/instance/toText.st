printing
toText
	"Return the FileUrl according to RFC1738 plus supporting fragments:
		'file://<host>/<path>#<fragment>'
	Note that <host> being '' is equivalent to 'localhost'.
	Note: The pathString can not start with a leading $/
	to indicate an 'absolute' file path.
	This is not according to RFC1738 where the path should have
	no leading or trailing slashes, and always
	be considered absolute relative to the filesystem."

	^String streamContents: [:s |
		s nextPutAll: self schemeName, '://'.
		host ifNotNil: [s nextPutAll: host].
		s nextPut: $/; nextPutAll: self pathString.
		fragment ifNotNil: [ s nextPut: $#; nextPutAll: fragment encodeForHTTP ]]