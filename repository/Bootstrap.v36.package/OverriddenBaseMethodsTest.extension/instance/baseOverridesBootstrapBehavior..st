*bootstrap
baseOverridesBootstrapBehavior: known
	"Behavior methods that get overridden when bootstrap mcz file is installed"

	self
		addTo: known
		forBehavior: Behavior
		overrides: {
			#'allSelectors' -> 'allSelectors

"Returns an Array of Symbols, consisting of all of the message
 selectors that instances of the receiver can understand, including
 those inherited from superclasses.  For keyword messages, the
 Symbol includes each of the keywords, concatenated together."

| result currClass |

(superClass == nil)
ifTrue:
   [^ self selectors]
ifFalse:
   [ result:= Array new.
     currClass:= self.
     [ currClass == nil ]
     whileFalse:
     [ result insert: (currClass selectors) at: 1.
       currClass:= currClass superClass.
     ].
   ].
^ result
'.
	}