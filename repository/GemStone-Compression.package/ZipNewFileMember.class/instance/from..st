initialization
from: aFileName
	compressionMethod := CompressionStored.
	"Now get the size, attributes, and timestamps, and see if the file exists"
	stream := self fileDirectoryClass readOnlyFileNamed: aFileName.
	self localFileName: (externalFileName := stream name).
	compressedSize := uncompressedSize := stream fileSize.
	desiredCompressionMethod := compressedSize > 0 ifTrue: [ CompressionDeflated ] ifFalse: [ CompressionStored ].
	self setLastModFileDateTimeFrom: stream lastModified