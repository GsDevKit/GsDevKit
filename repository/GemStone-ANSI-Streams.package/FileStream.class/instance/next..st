accessing
next: n
	"Return a the appropriate collection species with the next n characters/bytes of the filestream in it."

	| result charSize |
	result := self collectionSpecies new.
	charSize := self isBinary
		ifTrue: [ 1 ]
		ifFalse: [ result charSize ].
	(self gsfile next: n ofSize: charSize into: result ) == nil ifTrue:[ ^ nil ].
	^ result
