*gemstone-23x-ansi-streams
positionA

"Returns the receiver's current position reference for accessing the sequence
 of objects.  The position is actually the next element of the collection to be
 read or written; the position is incremented by each read or write.  In
 general, to start reading or writing at the nth element of a collection, the
 position must be set to n.
 
 This is the ANSI method. See Bug #39503."

^position - 1.
