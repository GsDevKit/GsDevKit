*squeak
_printAsDateAndTimeANSISecondsOn: aStream

	| string i |
	"Adding 100 causes a very tiny loss of precission but makes parsing much easier"
	string := (self + 100) asString.	
	(string at: 4) = $. ifFalse: [self error: 'unexpected format'].
	i := string size.
	[
		(string at: i) = $0 or: [ (string at: i) = $. ].
	] whileTrue: [
		i := i - 1.
	].
	aStream nextPutAll: (string copyFrom: 2 to: i).
