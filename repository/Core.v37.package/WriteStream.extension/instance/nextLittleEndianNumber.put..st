*core-squeak
nextLittleEndianNumber: n put: value
	| bytes |
	bytes := ByteArray new: n.
	1 to: n do: [: i | bytes at: i put: (value digitAt: i)].
	self nextPutAll: bytes