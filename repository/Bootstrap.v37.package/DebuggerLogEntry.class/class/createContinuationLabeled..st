continuation creation
createContinuationLabeled: aString
	"snap off a continuation at this point and stash it in the queue"
	
	self createContinuationFor: (self error: aString)