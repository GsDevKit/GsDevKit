as yet unclassified
initializeWithRoot: root leaf: leaf 
	| current |
	state := Array streamContents: 
		[:stream | 
		current := self childOf: root ancestorOf: leaf indexOn: stream.
		[current isNil or: [current = leaf]] whileFalse:
			[current := self childOf: current ancestorOf: leaf indexOn: stream].
		current ifNotNil: 
			[current metaNode hasChildren ifTrue: 
				[stream nextPut: current asFan; nextPut: 0]]].
	^ current ifNotNil: [self]