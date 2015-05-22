printing
toText
	| ans |
	ans _ WriteStream on: String new.
	ans nextPutAll: self schemeName.
	ans nextPutAll: '://'.
	self username ifNotNil: [
		ans nextPutAll: self username.
		self password ifNotNil: [
			ans nextPutAll: ':'.
			ans nextPutAll: self password ].
		ans nextPutAll: '@' ].
	ans nextPutAll: self authority.
	port ifNotNil: [ans nextPut: $:; print: port].
	path do: [ :pathElem |
		ans nextPut: $/.
		ans nextPutAll: pathElem encodeForHTTP. ].
	self query isNil ifFalse: [ 
		ans nextPut: $?.
		ans nextPutAll: self query. ].
	self fragment isNil ifFalse: [
		ans nextPut: $#.
		ans nextPutAll: self fragment encodeForHTTP. ].
	
	^ans contents