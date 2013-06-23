*gemstone-indexing-extensions
_evaluatorFor: aPath isRangeEqualityOperation: isRangeEqualityOperation

	| iList theIndexObj pathEvaluator |
	iList := self _indexedPaths.
	1 to: iList size by: 2 do: [ :i | | indexObj |
  		indexObj := iList at: i.
  		(iList at: i + 1) == 1
    			ifTrue: [
      			" see if the index matches the path "
      			(indexObj hasIndexOnPath: aPath)
        				ifTrue: [
          				" if range index found for range operation or
          				  identity index found for identity operation "
          				(( isRangeEqualityOperation and: [ indexObj isRangeEqualityIndex ]) or:
          						[ isRangeEqualityOperation not and: [ indexObj isIdentityIndex ]])
            					ifTrue: [ ^indexObj asQueryEvaluator ]
             					ifFalse: [ theIndexObj := indexObj ]]]].
   	"identity operations can be performed with any index (identity or equality) that has the same path"
 	(isRangeEqualityOperation not and: [ theIndexObj ~~ nil ])
		ifTrue: [ ^theIndexObj asQueryEvaluator ].
	"we'll have to create a PathEvaluator which performs the operation the hard way"
	pathEvaluator := PathEvaluator new.
	pathEvaluator nsc: self.
	aPath isEmpty
		ifTrue: [ pathEvaluator add: '' ]
		ifFalse: [ aPath do: [:sym | pathEvaluator add: sym ]].
	^pathEvaluator
	
