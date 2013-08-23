as yet unclassified
childOf: current ancestorOf: leaf indexOn: stream 
	| fan |
	fan := current asFan.
	stream nextPut: fan.
	^fan ancestorOf: leaf in: [:index | stream nextPut: index]