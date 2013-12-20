*bootstrap
precedence

"Returns the precedence of the receiver, , assuming it is a valid Smalltalk
	message selector or 0 otherwise:
 		1=unary, 2=binary and 3=keyword."

self size = 0 ifTrue: [^ 0].
^self isInfix
    ifTrue: [ 2 ]
    ifFalse: [ self isKeyword ifTrue: [ 3 ]
                              ifFalse: [ 1 ] ]