accessing
versionName
	"Answer my version name as a String, without the file suffix or any ancestor-attributes."
	| end |
	self isEmpty ifTrue: [^ String new]. 
	end := self indexOf: $( ifAbsent: [
		(self size > 4 
			and: [ (self at: self size - 3) == $.
			and: [ (self at: self size - 2) == $m
			and: [ (self at: self size - 1) == $c ] ] ])
				ifTrue: [self size - 3]
				ifFalse: [self size + 1]].
	^ self species withAll: (self first: end - 1)