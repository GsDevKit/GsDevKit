private
findStepPointIn: aGsMethod	
	| selectedOffset steps stepPoint prev |
	selectedOffset := target selection _from.
	stepPoint := 0.
	steps := Array new: aGsMethod _sourceString size.
	aGsMethod _sourceOffsets do: [:offset |
		stepPoint := stepPoint + 1.
		steps at: offset put:  stepPoint].
	prev := 0.
	1 to: steps size do: [:i | 
		stepPoint := steps at: i.
		stepPoint ~~ nil ifTrue: [
		(prev <= selectedOffset and: [selectedOffset <= i]) ifTrue: [ ^stepPoint ].
		prev := i]].
	^1