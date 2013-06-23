*gemstone-23x-ansi-streams
positionA: anInteger

"Sets the receiver's current position reference for accessing the collection to
 be anInteger.  If anInteger is not within the bounds of the collection,
 generates an error.
 
 This is the ANSI method. See Bug #39503."

	self positionL: anInteger + 1.
