accessing
value
	^ self object at: self key ifAbsent: [ nil ]