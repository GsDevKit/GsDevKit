accessing
nextPutAllBytes: aCharacterCollection
"Inserts the byte contents of aCharacterCollection as the next elements 
 that the receiver can access.  Returns aCollection.  The receiver's collection
 must be type of String."

	self gsfile nextPutAllBytes: aCharacterCollection