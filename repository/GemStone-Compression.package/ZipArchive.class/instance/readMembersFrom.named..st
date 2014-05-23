private
readMembersFrom: stream named: fileName
	| newMember signature |
	[
		newMember := self memberClass newFromZipFile: stream named: fileName.
		signature := self readSignatureFrom: stream.
		signature = EndOfCentralDirectorySignature ifTrue: [ ^self ].
		signature = CentralDirectoryFileHeaderSignature
			ifFalse: [ self error: 'bad CD signature at ', (stream position - 4) hex ].
		newMember readFrom: stream.
		"newMember looksLikeDirectory ifTrue: [ newMember := newMember asDirectory ]." "ZipFileDirectory not supported"
		self addMember: newMember.
	] repeat.