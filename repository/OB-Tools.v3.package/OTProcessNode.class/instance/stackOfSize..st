navigation
stackOfSize: anInteger 
	| current stack |
	[current := self context] on: Error do: [:ex | ^#() ].
	stack := OrderedCollection new.
	[ current notNil and: [ stack size < anInteger ] ] whileTrue: [
		stack addLast: (OBPlatform current otContextNodeClass on: self process context: current).
		current := current sender ].
	^ stack