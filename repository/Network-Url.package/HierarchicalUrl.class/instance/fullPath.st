printing
fullPath
	| ans |
	ans := WriteStreamPortable on: String new.
	path do: [ :pathElem |
		ans nextPut: $/.
		ans nextPutAll: pathElem encodeForHTTP. ].
	self query == nil  ifFalse: [ 
		ans nextPut: $?.
		ans nextPutAll: self query. ].
	self fragment == nil  ifFalse: [
		ans nextPut: $#.
		ans nextPutAll: self fragment encodeForHTTP. ].
	
	^ans contents
