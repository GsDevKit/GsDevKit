pool initialization
initializeWriteStreamConstants

	MaxBits _ 15.
	MaxBitLengthBits _ 7.
	EndBlock _ 256.

	StoredBlock _ 0.
	FixedBlock _ 1.
	DynamicBlock _ 2.

	NumLiterals _ 256.
	MaxLengthCodes _ 29.
	MaxDistCodes _ 30.
	MaxBitLengthCodes _ 19.
	MaxLiteralCodes _ NumLiterals + MaxLengthCodes + 1. "+ End of Block"

	Repeat3To6 _ 16. "Repeat previous bit length 3-6 times (2 bits repeat count)"
	Repeat3To10 _ 17. "Repeat previous bit length 3-10 times (3 bits repeat count)"
	Repeat11To138 _ 18. "Repeat previous bit length 11-138 times (7 bits repeat count)"

	self initializeExtraBits.
	self initializeLengthCodes.
	self initializeDistanceCodes.
	self initializeFixedTrees.