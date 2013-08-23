accessing
surroundingHierarchy
	| class remaining result |
	class := self theNonMetaClass.
	remaining := OrderedCollection with: class.
	result := class allSuperClasses asOrderedCollection.
	[ remaining isEmpty ] whileFalse: [
		result add: (class := remaining removeLast theNonMetaClass).
		class = Class
			ifFalse: [ remaining addAll: class subclasses ] ].
	^ result