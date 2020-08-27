*core-squeak
beginsWith: aSequenceableCollection

	(aSequenceableCollection isEmpty or: [self size < aSequenceableCollection size]) ifTrue: [^false].
	aSequenceableCollection withIndexDo: [:each :index | (self at: index) ~= each ifTrue: [^false]].
	^true