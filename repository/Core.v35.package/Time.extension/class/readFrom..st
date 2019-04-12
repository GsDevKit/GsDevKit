*core-squeak
readFrom: aStream

	"Read a Time from the stream in the form:
		<hour>:<minute>:<second> <am/pm>
	<minute>, <second> or <am/pm> may be omitted.  e.g. 1:59:30 pm; 8AM; 15:30"

	| hour minute second ampm totalSeconds |
	hour := Integer readFrom: aStream.
	minute := 0.
	second := 0.
	(aStream peekFor: $:) ifTrue:
	[ minute := Integer readFrom: aStream.
		(aStream peekFor: $:) ifTrue: [ second := Integer readFrom: aStream ]].
	aStream skipSeparators.
	(aStream atEnd not and: [aStream peek isLetter]) ifTrue: 
		[ampm := aStream next asLowercase.
	(ampm = $p and: [hour < 12]) ifTrue: [hour := hour + 12].
		(ampm = $a and: [hour = 12]) ifTrue: [hour := 0].
	(aStream peekFor: $m) ifFalse: [aStream peekFor: $M ]].

	totalSeconds := (hour * 3600) + (minute * 60) + second.
	^ self fromSeconds: totalSeconds .

	"Time readFrom: (ReadStream on: '2:23:09 pm')"