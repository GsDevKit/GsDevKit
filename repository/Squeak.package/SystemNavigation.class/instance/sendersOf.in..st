private
sendersOf: aSelector in: aclassSet
	"Returns an Array of GsMethods that send the given selector.  "

	| result cset cls |

	result := Array new.

	(aclassSet isKindOf: ClassSet) 
		ifFalse: [ cset := ClassSet withAll: aclassSet ]
		ifTrue: [ cset := aclassSet ].

	cset := cset sortAscending.
	1 to: cset size do: [ :i |
		cls := cset at: i.
		#[ cls _methodDict, cls class _methodDict ] do: [:methodDict | 
  			methodDict valuesDo:[ :gsMethod |
				((gsMethod _selectorPool includes: aSelector) 
					or: [gsMethod literals includes: aSelector])
						ifTrue:[ result add: gsMethod ]]]].

^result