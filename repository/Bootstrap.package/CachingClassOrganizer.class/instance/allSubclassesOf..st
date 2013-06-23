queries
allSubclassesOf: aClass

"Returns a collection of all the subclasses of the given class: an Array that
 holds a depth-first traversal of the class hierarchy subtree rooted at
 aClass."

| result subs |

classes == nil ifTrue: [
  self updateClassInfo
].
(classes includesIdentical: aClass) ifFalse: [
  self addClass: aClass.
  ^self allSubclassesOf: aClass
].

result := Array new.
subs := hierarchy at: aClass otherwise: nil.
subs size > 0 ifTrue: [
  subs do: [:each | | ar |
	ar := subHierarchy at: each otherwise: nil.
	ar == nil 
		ifTrue: [ 
			ar := Array new.
			ar add: each; addAll: (self allSubclassesOf: each).
			subHierarchy at: each put: ar ].
	result addAll: ar.
  ].
].
^result
