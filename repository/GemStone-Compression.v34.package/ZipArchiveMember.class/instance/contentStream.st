accessing
contentStream
	"Answer my contents as a text stream.
	Default is no conversion, since we don't know what the bytes mean."

	| s |
	s := RWBinaryOrTextStream on: (String new: self uncompressedSize).
	self extractTo: s.
	s reset.
	^ s.