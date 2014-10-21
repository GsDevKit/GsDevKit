accessing
customProjectAttributes
  | gsVersion projectAttributes |
  projectAttributes := #().
  ((gsVersion := System stoneVersionAt: 'gsVersion') beginsWith: '2.4')
    ifTrue: [ 
      (MetacelloVersionNumber fromString: gsVersion)
        < (MetacelloVersionNumber fromString: '2.4.4.2')
        ifTrue: [ 
          "needed to isolate the fix for Issue 129. The fix is needed for versions 2.4.4.1 and earlier"
          projectAttributes := #(#'Issue29FixANeeded') ]
        ifFalse: [ 
          "post 2.4.4.1 a slightly different patch required"
          projectAttributes := #(#'Issue29FixBNeeded') ] ].
  ((Smalltalk classNamed: 'MCClassDefinition') ifNotNil: [:cls | (cls canUnderstand: #traitCompositionString) not])
    ifTrue: [
      projectAttributes := projectAttributes copyWith: #'Issue3TraitLoadFixNeeded' ].
  ^ projectAttributes