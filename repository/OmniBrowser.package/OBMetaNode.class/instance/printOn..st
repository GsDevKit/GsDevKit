printing
printOn: aStream
	aStream nextPutAll: self name;cr.
	edges do: [:e | e printOn: aStream. aStream cr].