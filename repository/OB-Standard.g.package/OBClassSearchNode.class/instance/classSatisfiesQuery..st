private
classSatisfiesQuery: aClass
| classQueried |

	classQueried := aClass name.
	query do: [:aQuery |
	(classQueried includesSubstring: aQuery caseSensitive: false)
			ifTrue: [classQueried := classQueried 
						copyFrom: (classQueried indexOfSubCollection: aQuery) + aQuery size
						to: classQueried size]
			ifFalse:[^false]].
	^true