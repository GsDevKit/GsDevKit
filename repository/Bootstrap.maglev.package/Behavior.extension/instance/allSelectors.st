*bootstrap
allSelectors

"Returns an Array of Symbols, consisting of all of the message
 selectors that instances of the receiver can understand, including
 those inherited from superclasses.  For keyword messages, the
 Symbol includes each of the keywords, concatenated together."

 | result currClass |

 superClass ifNil:[ ^ self selectors] .
 result:= IdentitySet new.
 currClass:= self.
 [ currClass == nil ] whileFalse:[ 
   result addAll: currClass selectors . "includes session and base meths"
   currClass:= currClass superClass .
 ].
 ^ result asArray
