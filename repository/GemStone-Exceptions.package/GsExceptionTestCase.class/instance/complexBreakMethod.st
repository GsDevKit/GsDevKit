private
complexBreakMethod

	| coll |
	coll := OrderedCollection new.
	#(1 2 3 4 5) do: [:i | coll add: i printString ].
	self assert: true.
	self assert: (self asString isString)