*gssqueakcommon-core
waitTimeoutMSecs: anInteger
	"Wait on this semaphore for up to the given number of milliseconds, then timeout. 
	Return true if the deadline expired, false otherwise."

	^self waitForMilliseconds: anInteger