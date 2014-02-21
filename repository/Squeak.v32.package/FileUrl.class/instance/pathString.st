paths
pathString
	"Path as it appears in a URL with $/ as delimiter."
	
	| first |
	^String streamContents: [ :s |
		"isAbsolute ifTrue:[ s nextPut: $/ ]."
		first _ true.
		self path do: [ :p |
			first ifFalse: [ s nextPut: $/ ].
			first _ false.
			s nextPutAll: p encodeForHTTP ] ]