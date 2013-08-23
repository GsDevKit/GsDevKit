private
candidateClassesIn: anEnvironment pattern: pattern 
	| matching |
	matching := (self allBehaviorsIn: anEnvironment) select: (self selectorBlockFor: pattern).
	^ matching isEmpty
		ifTrue: [nil]
		ifFalse: [matching]