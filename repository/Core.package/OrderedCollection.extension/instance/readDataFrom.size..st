*core-squeak
readDataFrom: aDataStream size: varsOnDisk
	"OrderedCollection in Squeak is a different shape. First iv is an array, second iv is firstIndex and second iv is lastIndex."
	| cntInstVars cntIndexedVars ar firstIndex lastIndex |

	cntInstVars := self class instSize.
	self class isVariable
		ifTrue: [cntIndexedVars := varsOnDisk - cntInstVars.
				cntIndexedVars < 0 ifTrue: [
					self error: 'Class has changed too much.  Define a convertxxx method']]
		ifFalse: [cntIndexedVars := 0.
				cntInstVars := varsOnDisk]. 	"OK if fewer than now"

	aDataStream beginReference: self.
	ar := aDataStream next.
	firstIndex := aDataStream next.
	lastIndex := aDataStream next.
	firstIndex <= lastIndex
		ifTrue: [ 
			"reset size to correct value"
			self size: (lastIndex - firstIndex + 1).
			ar copyFrom: firstIndex count: (lastIndex - firstIndex + 1) into: self startingAt: 1.
		 ]
		ifFalse: [
			"empty OrderedCollection"
			self size: 0.
		].
	^ self