*core-squeak
squeakMatch: aString
	"# and * are the special characters"
	| special pattern keyStart keyStop char |
	special := #[ $#, $*].
	
	pattern := OrderedCollection new.	
	keyStart := keyStop := 1.
	[keyStop <= self size] whileTrue:
		[keyStop _ self findDelimiters: special startingAt: keyStart.
		keyStart <= keyStop
			ifTrue: [
				keyStart = keyStop
					ifTrue: [ keyStart := keyStart + 1 ]
					ifFalse: [ 
						pattern add: (self copyFrom: keyStart to: (keyStop - 1)).
						keyStart := keyStop + 1. ].
				(keyStop <= self size) 
					ifTrue: [
						char := self at: keyStop.
						char == $# ifTrue: [ char := $? ].
						pattern add: char ]]].
	^aString _matchPatternNoCase: pattern asArray