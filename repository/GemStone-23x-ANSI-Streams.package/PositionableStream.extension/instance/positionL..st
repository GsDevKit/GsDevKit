*gemstone-23x-ansi-streams
positionL: anInteger

"Sets the receiver's current position reference for accessing the collection to
 be anInteger.  If anInteger is not within the bounds of the collection,
 generates an error.
 
 This is the 'Legacy' (non-ANSI) method. See Bug #39503."

(anInteger > 0) & (anInteger <= (itsCollection size + 1))
   ifTrue: [position := anInteger]
   ifFalse: [self _positionError: anInteger]
