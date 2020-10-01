comparing
hash

	| hashValue |
	hashValue := behavior hash.
	hashValue := hashValue bitXor: selector hash.
	hashValue := hashValue bitXor: category hash.
	hashValue := hashValue bitXor: changeStamp hash.
	hashValue := hashValue bitXor: source hash.
	^hashValue abs