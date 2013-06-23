private
uncompressDataTo: aStream

	| compressedBytes unCompressedBytes pos | 
	pos := stream position.
	compressedBytes := (stream contents copyFrom: pos to: (pos + compressedSize)) asByteArray.
	unCompressedBytes := compressedBytes _decompressBytes.
	unCompressedBytes == nil ifTrue: [ ^self stUncompressDataTo: aStream ].
	self crc32 ~= (unCompressedBytes _computeCRC32: 0) ifTrue: [ self error: 'Throw CRCError here' ].
	aStream nextPutAll: unCompressedBytes.