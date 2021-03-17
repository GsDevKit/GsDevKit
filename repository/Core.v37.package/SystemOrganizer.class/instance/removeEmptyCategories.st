accessing
removeEmptyCategories

	| removals |
	removals := Array new.
	self categoryDict keysAndValuesDo: [:catName :elements |
		elements isEmpty ifTrue: [ removals add: catName ].
	].
	removals do: [:catName | self removeSystemCategory: catName ].