*core-squeak
storeDataOn: aDataStream
	"OrderedCollection in Squeak is a different shape. First iv is an array, second iv is firstIndex and second iv is lastIndex."

	aDataStream
		beginInstance: self class
		size: 3.
	"array"
	aDataStream nextPut: self asArray.
	"firstIndex"
	aDataStream nextPut: 1.
	"lastIndex"
	aDataStream nextPut: self size.