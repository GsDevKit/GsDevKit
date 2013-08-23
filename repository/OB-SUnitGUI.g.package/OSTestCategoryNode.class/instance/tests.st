accessing
tests
	| nodes |
	nodes := OrderedCollection new.
	self coParents do: [:aParent |
		nodes addAll: (((((Smalltalk organization listAtCategoryNamed: aParent name)
				collect: [ :each | Smalltalk at: each ])
					select: [ :each | each includesBehavior: aParent baseClass ] )  asArray
						sort: [ :a :b | self sortClass: a before: b ]) collect: [:ea | (OSTestNode on: ea) root: root; yourself ])].
	^nodes