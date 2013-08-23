exceptionInstantiator
message: aString
	^ (self new
		message: aString;
		yourself) signal