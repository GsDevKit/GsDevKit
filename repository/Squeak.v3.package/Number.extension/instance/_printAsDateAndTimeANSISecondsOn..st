*squeak
_printAsDateAndTimeANSISecondsOn: aStream

	self \\ 1 = 0 ifTrue: [
		self < 10 ifTrue: [aStream nextPut: $0].
		self rounded printOn: aStream.
	] ifFalse: [	"See bugs 36718, 40624, and 40643"
		| string i |
		"Adding 100 causes a very tiny loss of precission but makes parsing much easier"
		string := (self + 100) asFloat asStringLocaleC.	
		(string at: 2) = $. ifFalse: [self error: 'unexpected format'].
		(string copyFrom: string size - 3 to: string size) = 'E+02' ifFalse: [self error: 'unexpected format'].
		i := string size - 4.
		[
			(string at: i) = $0.
		] whileTrue: [
			i := i - 1.
		].
		aStream
			nextPut: (string at: 3);
			nextPut: (string at: 4);
			nextPut: $.;
			nextPutAll: (string copyFrom: 5 to: i).
	].