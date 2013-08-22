private
isClassQuery: aString 
	^ (aString first = $* and: [ aString size > 1 and: [ aString second ~= $* ] ]) or: [ (aString allButLast indexOf: $*) > 1 or: [ aString first isUppercase ] ]