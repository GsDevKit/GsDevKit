*bootstrap
allSelectors

"Returns an Array of Symbols, consisting of all of the message
 selectors that instances of the receiver can understand, including
 those inherited from superclasses.  For keyword messages, the
 Symbol includes each of the keywords, concatenated together."

| result currClass meths |

(superClass == nil)
ifTrue:
   [^ self selectors]
ifFalse:
   [ result:= Set new.
     currClass:= self.
     [ currClass == nil ]
     whileFalse:
     [ meths := currClass selectors asSet.
       GsPackagePolicy current selectorsFor: currClass into: meths.
       result addAll: meths.
       currClass:= currClass superClass.
     ].
   ].
^ result asArray
