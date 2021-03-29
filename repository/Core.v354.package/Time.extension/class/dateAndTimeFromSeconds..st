*core-squeak
dateAndTimeFromSeconds: secondCount
	| dateAndTime |
	dateAndTime := DateAndTime fromSeconds: secondCount.
	^ Array
		with: dateAndTime asDate
		with: dateAndTime asTime