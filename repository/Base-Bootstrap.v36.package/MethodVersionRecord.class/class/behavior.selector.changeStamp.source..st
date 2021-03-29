instance creation
behavior: aBehavior selector: aSelector changeStamp: aChangeStamp source: aString

	^self new 
		behavior: aBehavior;
		selector: aSelector;
		changeStamp: aChangeStamp;
		source: aString;
		yourself