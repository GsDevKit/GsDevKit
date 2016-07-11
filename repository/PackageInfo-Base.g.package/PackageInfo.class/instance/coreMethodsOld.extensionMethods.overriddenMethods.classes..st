listing
coreMethodsOld: coreMethodBlock extensionMethods: extensionMethodBlock overriddenMethods: overriddenMethodBlock classes: classesBlock
  "optimization for doing a snapshot"

  | myBehaviors myClasses |
  myBehaviors := IdentitySet new.
  myClasses := IdentitySet new.
  self systemCategories
    do: [ :cat | 
      (SystemOrganizer new listAtCategoryNamed: cat)
        do: [ :className | 
          | cl |
          cl := Smalltalk classNamed: className.
          myClasses add: cl.
          myBehaviors
            add: cl;
            add: cl class ] ].
  myBehaviors
    do: [ :beh | 
      (self coreMethodsForClass: beh)
        do: [ :m | 
          (m isValid and: [ (#(#'DoIt' #'DoItIn:') includes: m methodSymbol) not ])
            ifTrue: [ coreMethodBlock value: m ] ] ].
  MCPlatformSupport
    allBehaviorsDo: [ :beh | 
      | isExtensionBehavior |
      isExtensionBehavior := (myBehaviors includes: beh) not.
      beh
        env: 0
        categorysDo: [ :cat :selectors | 
          (self isOverrideCategory: cat)
            ifTrue: [ 
              (self methodsInCategory: cat ofClass: beh)
                do: [ :m | 
                  (self isOverrideOfYourMethod: m)
                    ifTrue: [ overriddenMethodBlock value: m ] ] ].
          isExtensionBehavior
            ifTrue: [ 
              (self isYourClassExtension: cat)
                ifTrue: [ 
                  (self methodsInCategory: cat ofClass: beh)
                    do: [ :m | 
                      (m isValid and: [ (#(#'DoIt' #'DoItIn:') includes: m methodSymbol) not ])
                        ifTrue: [ extensionMethodBlock value: m ] ] ] ] ] ].
  myClasses do: classesBlock