writing
writeTo: stream
	members do: [ :member |
		member writeTo: stream.
		member endRead.
	].
	writeCentralDirectoryOffset := stream position - 1.
	self writeCentralDirectoryTo: stream.
	