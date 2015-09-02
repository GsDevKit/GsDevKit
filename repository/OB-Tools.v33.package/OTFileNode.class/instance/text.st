accessing
text
	| stream contents |
	stream := self directory readOnlyFileNamed: self name.
	contents := [ [ stream next: 10000 ]
		ensure: [ stream close ] ]
			on: Error
			do: [ :err | err messageText ].
	contents size = 10000
		ifTrue: [ contents := contents , '...' ].
	^ contents