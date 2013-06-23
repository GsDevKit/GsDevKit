query
allClassesImplementing: aSelector  
	"Answer an Array of all classes that implement the message aSelector."

	| result cset cls gsMethod |

	result := Array new.
	cset := self allClasses.
	1 to: cset size do: [ :j |
		cls := cset at: j.
		gsMethod := cls _methodDict at: aSelector otherwise: nil .
		gsMethod == nil 
			ifTrue: [
				gsMethod := cls class _methodDict at: aSelector otherwise: nil .
				gsMethod ~~ nil ifTrue: [ result add: cls class ].
			]
			ifFalse: [ result add: cls ].
	].

	^result