as yet unclassified
mimeEncode: aStream
	"Return a ReadWriteStream of characters.  The data of aStream is encoded as 65 innocuous characters.  (See class comment). 3 bytes in aStream goes to 4 bytes in output."

	| me |
	aStream setToBeginning.
	me := self new dataStream: aStream.
	me mimeStream: (ReadWriteStream on: String new).
	me mimeEncode.
	me mimeStream setToBeginning.
	^ me mimeStream