navigation
childInspectorNodes

	| result items |
	result := OrderedCollection new.
	[(Pragma
		allNamed: #inspector:priority:
		from: self value class to: nil
		sortedByArgument: 2) do: [ :pragma |
			items := self value perform: pragma selector.
			items isEmptyOrNil ifFalse: [ 
				items do: [ :each | 
					each metaNode: metaNode; tag: (pragma argumentAt: 1) asString ].				
				result addAll: items ] ]] on: Error do: [:ex | ^#()].
	^ result