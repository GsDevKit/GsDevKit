accessing
nextPutAll: aCollection

"Inserts the elements of aCollection as the next elements that the receiver can
 access.  Returns aCollection."

	aCollection do: [:each | self nextPut: each ]