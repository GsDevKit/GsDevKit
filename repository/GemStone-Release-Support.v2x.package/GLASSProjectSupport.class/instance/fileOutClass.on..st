bootstrap fileout
fileOutClass: cl on: stream

	| def |
	def := cl definition.
	stream 
		nextPutAll: 'doit'; nextPut: Character lf;
     	nextPutAll: (def copyFrom: 1 to: def size - 1); nextPut: $.; nextPut: Character lf;
		nextPutAll: 'true'; nextPut: Character lf;
      	nextPut: $%; nextPut: Character lf