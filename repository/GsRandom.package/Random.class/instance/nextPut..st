accessing
nextPut: anObject
 "Random numbers do not implement nextPut: so provide an
 error notification."

 ^self shouldNotImplement: #nextPut: