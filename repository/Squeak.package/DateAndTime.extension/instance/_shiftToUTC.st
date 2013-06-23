*squeak
_shiftToUTC
	"Used in tests, to shift the receiver such that it maintains the same clock time for UTC.
	 Used to take messages that create instances of DateAndTime with local offsets and allow
	 the receiver to be shifted to UTC (with 0 offset)...used mainly when comparing DateAndTime
	 printStrings."

	^self class
		secondsLocal: seconds + offset
		offset: Duration zero.
