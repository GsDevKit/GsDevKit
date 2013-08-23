private
writeMethodHeaderTo: aStream
	|keywords|
	self selector numArgs = 0 ifTrue: [aStream nextPutAll: self selector. ^ self].
	
	keywords := self selector keywords.
	(1 to: keywords size)
       do: [:i |
			aStream
				nextPutAll: (keywords at: i);
				space;
				nextPutAll: 'arg';
				print: i
		 ]
	separatedBy: [aStream space]