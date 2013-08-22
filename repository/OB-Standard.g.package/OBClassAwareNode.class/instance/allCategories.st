navigating
allCategories
	"Return all categories of theClass and its superclasses."
	| categories allClasses |
	categories := IdentitySet new.
	allClasses := self theClass withAllSuperclasses.
	(self theClass isKindOf: Metaclass)
		ifTrue: [ allClasses := allClasses copyUpTo: Class ].
	allClasses do: [:cls | categories addAll: cls categoryNames].
	categories := categories sortAscending.
	^ (OrderedCollection new)
		add: (OBAllMethodCategoryNode on: self theClass);
		addAll: (categories 
					collect: [:cat | OBMethodCategoryNode on: cat inClass: self theClass]);
		yourself