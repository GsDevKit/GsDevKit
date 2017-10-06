*core
baseOverridesCoreClass: known
	self
		addTo: known
		forBehavior: Class
		overrides: {
			"2.4.4"
			#'_subclass:instVarNames:format:constraints:classVars:classInstVars:poolDictionaries:inDictionary:inClassHistory:description:isModifiable:' -> '_subclass: className
instVarNames: anArrayOfStrings
format: theFormat
constraints: theConstraints
classVars: anArrayOfClassVars
classInstVars: anArrayOfClassInstVars
poolDictionaries: anArrayOfPoolDicts
inDictionary: aDictionary
inClassHistory: aClassHistory
description: aDescription
isModifiable: modifyBoolean

"The preferred private subclass creation method."

| cvDict result theName ivNames classCon temp aString conEle conEleEle theHist
  selfClass resultClass poolDicts |
self _validatePrivilege.
className _validateClass: CharacterCollection .
modifyBoolean _validateClass: Boolean .
anArrayOfClassInstVars ~~ nil ifTrue:[
  anArrayOfClassInstVars _validateClass: Array . "fix bug 11833"
  ].
theName := className asSymbol.
ivNames := anArrayOfStrings class new.
1 to: anArrayOfStrings size do: [:j |
  aString := anArrayOfStrings at: j .
  ivNames add: aString asSymbol
].
(theConstraints isKindOf: Array)
ifFalse: [ classCon:= theConstraints ]
ifTrue: [
   classCon:= theConstraints class new.
   1 to: theConstraints size do: [:j |
      conEle := theConstraints at: j .
      (conEle isKindOf: Array)
      ifFalse: [ classCon add: conEle ]
      ifTrue: [
         temp:= conEle class new.
         1 to: conEle size do: [:k |
            conEleEle := conEle at: k .
            (conEleEle isKindOf: CharacterCollection)
                 ifTrue: [temp add: conEleEle asSymbol ]
                ifFalse: [temp add: conEleEle].
                     ].
         classCon add: temp .
         ] .
      ] .
  ] .

cvDict:= self _makeClassVarDict: anArrayOfClassVars .

"undo the compiler''s canonicalization of empty arrays (fix bug 14103) "
poolDicts := anArrayOfPoolDicts .
poolDicts == #() ifTrue:[ poolDicts := poolDicts copy ].

result :=  aDictionary at: theName
           put:(  self _subclass: theName
                       instVarNames: ivNames
                       format: theFormat
                       constraints: classCon
                       classVars: cvDict
                       poolDictionaries: poolDicts ) .
modifyBoolean ifTrue:[
  result _subclasses: IdentitySet new .
  ] .
subclasses ~~ nil ifTrue:[ subclasses add: result ].

result description: aDescription .
theHist := aClassHistory .
theHist == nil
  ifTrue: [ theHist := ClassHistory new name: className ] .
theHist add: result .
result classHistory: theHist .
result timeStamp: DateTime now.
result userId: System myUserProfile userId.
result extraDict: SymbolDictionary new .

" if superclasses have class instance variables defined "
(selfClass := self class) instSize > (resultClass := result class) instSize
  ifTrue: [ 
    resultClass instSize + 1 to: selfClass instSize do: [ :i |
      resultClass addInstVarNames: #[ selfClass _instVarNames at: i ]
    ]
  ].
anArrayOfClassInstVars size > 0 ifTrue: [
  resultClass addInstVarNames: anArrayOfClassInstVars
].

modifyBoolean ifFalse:[ result immediateInvariant ] .
result copyVariables.
^ result
'.
			"2.3.x"
			#'_subclass:instVarNames:format:constraints:classVars:classInstVars:poolDictionaries:inDictionary:inClassHistory:description:isModifiable:' -> '_subclass: className
instVarNames: anArrayOfStrings
format: theFormat
constraints: theConstraints
classVars: anArrayOfClassVars
classInstVars: anArrayOfClassInstVars
poolDictionaries: anArrayOfPoolDicts
inDictionary: aDictionary
inClassHistory: aClassHistory
description: aDescription
isModifiable: modifyBoolean

"The preferred private subclass creation method."

| cvDict result theName ivNames classCon temp aString conEle conEleEle theHist
  selfClass resultClass poolDicts |
self _validatePrivilege.
className _validateClass: CharacterCollection .
anArrayOfClassInstVars ~~ nil ifTrue:[
  anArrayOfClassInstVars _validateClass: Array . "fix bug 11833"
  ].
theName := className asSymbol.
ivNames := anArrayOfStrings class new.
1 to: anArrayOfStrings size do: [:j |
  aString := anArrayOfStrings at: j .
  ivNames add: aString asSymbol
].
(theConstraints isKindOf: Array)
ifFalse: [ classCon:= theConstraints ]
ifTrue: [
   classCon:= theConstraints class new.
   1 to: theConstraints size do: [:j |
      conEle := theConstraints at: j .
      (conEle isKindOf: Array)
      ifFalse: [ classCon add: conEle ]
      ifTrue: [
         temp:= conEle class new.
         1 to: conEle size do: [:k |
            conEleEle := conEle at: k .
            (conEleEle isKindOf: CharacterCollection)
                 ifTrue: [temp add: conEleEle asSymbol ]
                ifFalse: [temp add: conEleEle].
                     ].
         classCon add: temp .
         ] .
      ] .
  ] .

cvDict:= self _makeClassVarDict: anArrayOfClassVars .

"undo the compiler''s canonicalization of empty arrays (fix bug 14103) "
poolDicts := anArrayOfPoolDicts .
poolDicts == #() ifTrue:[ poolDicts := poolDicts copy ].

result :=  aDictionary at: theName
           put:(  self _subclass: theName
                       instVarNames: ivNames
                       format: theFormat
                       constraints: classCon
                       classVars: cvDict
                       poolDictionaries: poolDicts ) .
modifyBoolean ifTrue:[
  result _subclasses: IdentitySet new .
  ] .
subclasses ~~ nil ifTrue:[ subclasses add: result ].

result description: aDescription .
theHist := aClassHistory .
theHist == nil
  ifTrue: [ theHist := ClassHistory new name: className ] .
theHist add: result .
result classHistory: theHist .
result timeStamp: DateTime now.
result userId: System myUserProfile userId.
result extraDict: SymbolDictionary new .

" if superclasses have class instance variables defined "
(selfClass := self class) instSize > (resultClass := result class) instSize
  ifTrue: [ 
    resultClass instSize + 1 to: selfClass instSize do: [ :i |
      resultClass addInstVarNames: #[ selfClass _instVarNames at: i ]
    ]
  ].
anArrayOfClassInstVars size > 0 ifTrue: [
  resultClass addInstVarNames: anArrayOfClassInstVars
].

modifyBoolean ifFalse:[ result immediateInvariant ] .

^ result
'.
	}