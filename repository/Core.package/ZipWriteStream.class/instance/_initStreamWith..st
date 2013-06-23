initialize-release
_initStreamWith: aCollectionOrStream
	crcPosition := 1.
	bytesWritten := 0.
	encoder := ZipEncoder on: aCollectionOrStream.
	self initialize.
	encoder isBinary
		ifTrue:[super _initStreamWith: (ByteArray new: WindowSize * 2)]
		ifFalse:[super _initStreamWith: (String new: WindowSize * 2)].
	crc := 16rFFFFFFFF.
	writeLimit := itsCollection size.
	self writeHeader.