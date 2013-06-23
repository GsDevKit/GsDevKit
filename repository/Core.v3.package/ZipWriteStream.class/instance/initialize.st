initialize-release
initialize
	literals := ByteArray new: WindowSize.
	distances := WordArray new: WindowSize.
	literalFreq := WordArray new: MaxLiteralCodes.
	distanceFreq := WordArray new: MaxDistCodes.
	self initializeNewBlock.
	blockStart = nil.
	blockPosition := 0.
	hashValue := 0.
	self initializeHashTables.