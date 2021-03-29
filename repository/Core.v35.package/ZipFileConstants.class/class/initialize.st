pool initialization
initialize
	"ZipFileConstants initialize"
	FaMsdos		:= 0.
	FaUnix 		:= 3.
	DeflatingCompressionNormal		:= 0.
	DeflatingCompressionMaximum	:= 2.
	DeflatingCompressionFast		:= 4.
	DeflatingCompressionSuperFast	:= 6.
	CompressionStored				:= 0.
	CompressionDeflated				:= 8.
	CompressionLevelNone			:= 0.
	CompressionLevelDefault			:= 6.
	IfaTextFile						:= 1.
	IfaBinaryFile					:= 0.
	DataDescriptorLength 				:= 12.

	"Unix permission bits"
	DefaultDirectoryPermissions		:= 8r040755.
	DefaultFilePermissions			:= 8r0100666.
	DirectoryAttrib 					:= 8r040000.
	FileAttrib 						:= 8r0100000.

	CentralDirectoryFileHeaderSignature _ 
		(ByteArray with: 16r50 with: 16r4B with: 16r01 with: 16r02).
	LocalFileHeaderSignature _
		(ByteArray with: 16r50 with: 16r4B with: 16r03 with: 16r04).
	EndOfCentralDirectorySignature _
		(ByteArray with: 16r50 with: 16r4B with: 16r05 with: 16r06).