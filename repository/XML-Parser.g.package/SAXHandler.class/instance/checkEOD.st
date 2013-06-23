content
checkEOD
	"Check if the document shouldn't be ended already"
	self eod
		ifTrue: [self driver errorExpected: 'No more data expected,']