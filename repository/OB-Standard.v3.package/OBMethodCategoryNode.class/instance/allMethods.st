navigating
allMethods
    | allMethods allClasses |
    allMethods := IdentitySet new.
    allClasses := self theClass withAllSuperclasses.
    (self theClass isMeta )
        ifTrue: [ allClasses := allClasses copyUpTo: Class ].
    allClasses do: [:cls | 
        allMethods addAll: (self selectorsFor: cls)].
    allMethods := allMethods asOrderedCollection.
    allMethods := allMethods sortAscending.
    ^ allMethods collect: [:selector | | class |
                    class := self theClass whichClassIncludesSelector:  selector.
                    (OBMethodNode on: selector inClass: class)
							symbolDictionary: self symbolDictionary;
							yourself ].
                    
