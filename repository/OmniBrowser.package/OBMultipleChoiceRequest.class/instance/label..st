accessing
label: anObject
	^ ((selection includes: anObject) ifTrue: [ '<yes>' ] ifFalse: [ '<no>' ]) , 
	  (labels at: (values indexOf: anObject ifAbsent: [ ^ nil ]))