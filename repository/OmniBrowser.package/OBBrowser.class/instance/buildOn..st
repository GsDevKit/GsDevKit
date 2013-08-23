building
buildOn: aBuilder 
	| group |
	^aBuilder window: self
		with: 
			[group := OrderedCollection with: panels first.
			panels allButFirst do: 
					[:panel | 
					group last vResizing = #spaceFill 
						ifTrue: 
							[self buildGroup: group on: aBuilder.
							group := OrderedCollection new].
					group add: panel].
			self buildGroup: group on: aBuilder]