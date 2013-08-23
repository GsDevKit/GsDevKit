accessing
value: anObject
	self object
		remove: anObject ifAbsent: [ ];
		add: anObject