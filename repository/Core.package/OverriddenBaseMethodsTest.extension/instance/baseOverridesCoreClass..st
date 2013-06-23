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
			"2.4.5"
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

" Fix 41763 and store a copy of the description"
aDescription == nil
  ifFalse:[ | newDesc |
            newDesc := aDescription copy .
            (newDesc isKindOf: GsClassDocumentation)
              ifTrue:[ newDesc itsClass: result ] .
result description: newDesc .
].

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
	};
		addTo: known 
		forBehavior: Class 
		overrides: {
			#copyVariables -> 'copyVariables
		"bug #40559 - copy class (instance) variables to new version"

	| priorVersion |
	classHistory size = 1 ifTrue: [^self].
	priorVersion := classHistory at: classHistory size - 1.
	priorVersion.classVars associationsDo: [:anAssociation |
		(classVars includesKey: anAssociation key) ifTrue: [
			classVars removeKey: anAssociation key.
			classVars addAssociation: anAssociation.
		].
	].
	priorVersion class instVarNames do: [:each | 
		(self class instVarNames includes: each) ifTrue: [
			self atClassInstVar: each put: (priorVersion atClassInstVar: each).
		].
	].
'.
			"2.4.5"
			#copyVariables -> 'copyVariables
| chSize chist priorVersion priorVars civNames priorCivNames toIgnoreCount |
(chSize := (chist := classHistory) size) = 1 ifTrue:[ 
  ^ self
].
priorVersion := chist at: chSize - 1.
(priorVars := priorVersion _classVars) notNil ifTrue: [ 
  | cvars |
  (cvars := classVars) notNil ifTrue: [
    priorVars associationsDo: [:anAssociation | |aKey |
      (cvars includesKey: (aKey := anAssociation key)) ifTrue: [
        cvars removeKey: aKey .
        cvars addAssociation: anAssociation.
      ].
    ].
  ].
].
priorCivNames := priorVersion class allInstVarNames.
toIgnoreCount := Class allInstVarNames size.
toIgnoreCount < priorCivNames size ifTrue: [
	priorCivNames := priorCivNames 
		copyFrom: toIgnoreCount + 1
		to: priorCivNames size.
	civNames := self class allInstVarNames.
	priorCivNames do: [:each | 
		(civNames includesIdentical: each) ifTrue: [
			self atClassInstVar: each put: (priorVersion atClassInstVar: each).
		].
	].
].
'.
			"2.3.1"
		#'subclass:instVarNames:classVars:classInstVars:poolDictionaries:inDictionary:constraints:instancesInvariant:newVersionOf:isModifiable:' ->'subclass: aString
instVarNames: anArrayOfStrings
classVars: anArrayOfClassVars
classInstVars: anArrayOfClassInstVars
poolDictionaries: anArrayOfPoolDicts
inDictionary: aDictionary
constraints: aConstraint
instancesInvariant: invarBoolean
newVersionOf: oldClass
isModifiable: modifyBoolean

"Obsolete in 2.1.  The preferred class creation methods are in the category
 Subclass Creation; see the similar method without the constraints keyword."

"Creates and returns a new subclass of the receiver.

 If oldClass is visible to the current user, this method creates the new class
 as a new version of oldClass, and the two classes then share the same class
 history.  However, if oldClass is nil, then the new class is no relation
 to any existing class, and it has a new class history."

self _validatePrivilege.
^ self subclass: aString
    instVarNames: anArrayOfStrings
    classVars: anArrayOfClassVars
    classInstVars: anArrayOfClassInstVars
    poolDictionaries: anArrayOfPoolDicts
    inDictionary: aDictionary
    constraints: aConstraint
    instancesInvariant: invarBoolean
    newVersionOf: oldClass
    description: nil
    isModifiable: modifyBoolean

'.
			"2.4.5"
		#'subclass:instVarNames:classVars:classInstVars:poolDictionaries:inDictionary:constraints:instancesInvariant:newVersionOf:isModifiable:' ->'subclass: aString
instVarNames: anArrayOfStrings
classVars: anArrayOfClassVars
classInstVars: anArrayOfClassInstVars
poolDictionaries: anArrayOfPoolDicts
inDictionary: aDictionary
constraints: aConstraint
instancesInvariant: invarBoolean
newVersionOf: oldClass
isModifiable: modifyBoolean

"Obsolete in 2.1.  The preferred class creation methods are in the category
 Subclass Creation; see the similar method without the constraints keyword."

"Creates and returns a new subclass of the receiver.

 If oldClass is visible to the current user, this method creates the new class
 as a new version of oldClass, and the two classes then share the same class
 history.  However, if oldClass is nil, then the new class is no relation
 to any existing class, and it has a new class history."

| desc |
self _validatePrivilege.
desc := oldClass == nil ifTrue:[nil] ifFalse:[ oldClass description].
^ self subclass: aString
    instVarNames: anArrayOfStrings
    classVars: anArrayOfClassVars
    classInstVars: anArrayOfClassInstVars
    poolDictionaries: anArrayOfPoolDicts
    inDictionary: aDictionary
    constraints: aConstraint
    instancesInvariant: invarBoolean
    newVersionOf: oldClass
    description: desc
    isModifiable: modifyBoolean

'.
			"2.4.4"
		#'subclass:instVarNames:classVars:classInstVars:poolDictionaries:inDictionary:instancesInvariant:newVersionOf:isModifiable:' -> 'subclass: aString
instVarNames: anArrayOfStrings
classVars: anArrayOfClassVars
classInstVars: anArrayOfClassInstVars
poolDictionaries: anArrayOfPoolDicts
inDictionary: aDictionary
instancesInvariant: invarBoolean
newVersionOf: oldClass
isModifiable: modifyBoolean

"Creates and returns a new subclass of the receiver.

 If oldClass is visible to the current user, this method creates the new class
 as a new version of oldClass, and the two classes then share the same class
 history.  However, if oldClass is nil, then the new class is no relation
 to any existing class, and it has a new class history."

self _validatePrivilege.
^ self subclass: aString
    instVarNames: anArrayOfStrings
    classVars: anArrayOfClassVars
    classInstVars: anArrayOfClassInstVars
    poolDictionaries: anArrayOfPoolDicts
    inDictionary: aDictionary
    constraints: #()
    instancesInvariant: invarBoolean
    newVersionOf: oldClass
    description: (oldClass isNil ifTrue: [nil] ifFalse: [oldClass description])
    isModifiable: modifyBoolean
'.
			"2.3.1"
		#'subclass:instVarNames:classVars:classInstVars:poolDictionaries:inDictionary:instancesInvariant:newVersionOf:isModifiable:' -> 'subclass: aString
instVarNames: anArrayOfStrings
classVars: anArrayOfClassVars
classInstVars: anArrayOfClassInstVars
poolDictionaries: anArrayOfPoolDicts
inDictionary: aDictionary
instancesInvariant: invarBoolean
newVersionOf: oldClass
isModifiable: modifyBoolean

"Creates and returns a new subclass of the receiver.

 If oldClass is visible to the current user, this method creates the new class
 as a new version of oldClass, and the two classes then share the same class
 history.  However, if oldClass is nil, then the new class is no relation
 to any existing class, and it has a new class history."

self _validatePrivilege.
^ self subclass: aString
    instVarNames: anArrayOfStrings
    classVars: anArrayOfClassVars
    classInstVars: anArrayOfClassInstVars
    poolDictionaries: anArrayOfPoolDicts
    inDictionary: aDictionary
    constraints: #()
    instancesInvariant: invarBoolean
    newVersionOf: oldClass
    description: nil
    isModifiable: modifyBoolean
'.
			"2.4.5"
		#'subclass:instVarNames:classVars:classInstVars:poolDictionaries:inDictionary:instancesInvariant:newVersionOf:isModifiable:' -> 'subclass: aString
instVarNames: anArrayOfStrings
classVars: anArrayOfClassVars
classInstVars: anArrayOfClassInstVars
poolDictionaries: anArrayOfPoolDicts
inDictionary: aDictionary
instancesInvariant: invarBoolean
newVersionOf: oldClass
isModifiable: modifyBoolean

"Creates and returns a new subclass of the receiver.

 If oldClass is visible to the current user, this method creates the new class
 as a new version of oldClass, and the two classes then share the same class
 history.  However, if oldClass is nil, then the new class is no relation
 to any existing class, and it has a new class history."

| desc |
self _validatePrivilege.
desc := oldClass == nil ifTrue:[nil] ifFalse:[ oldClass description].
^ self subclass: aString
    instVarNames: anArrayOfStrings
    classVars: anArrayOfClassVars
    classInstVars: anArrayOfClassInstVars
    poolDictionaries: anArrayOfPoolDicts
    inDictionary: aDictionary
    constraints: #()
    instancesInvariant: invarBoolean
    newVersionOf: oldClass
    description: desc
    isModifiable: modifyBoolean
'.
			"2.3.1"
		#'subclass:instVarNames:classVars:classInstVars:poolDictionaries:inDictionary:constraints:instancesInvariant:isModifiable:' -> 'subclass: aString
instVarNames: anArrayOfStrings
classVars: anArrayOfClassVars
classInstVars: anArrayOfClassInstVars
poolDictionaries: anArrayOfPoolDicts
inDictionary: aDictionary
constraints: aConstraint
instancesInvariant: invarBoolean
isModifiable: modifyBoolean

"Obsolete in 2.1  The perferred methods are in the category Subclass Creation;
 see the similar method without the constraints keyword."

"Creates and returns a new subclass of the receiver.

 If aString is the name of a Class that is visible to the current user, this
 method creates the new class as a new version of the existing class, and they
 then share the same class history.  However, if no class named aString is
 visible to the user, then the new class is no relation to any existing class,
 and it has a new class history."

self _validatePrivilege.
^self
  subclass: aString
  instVarNames: anArrayOfStrings
  classVars: anArrayOfClassVars
  classInstVars: anArrayOfClassInstVars
  poolDictionaries: anArrayOfPoolDicts
  inDictionary: aDictionary
  constraints: aConstraint
  instancesInvariant: invarBoolean
  newVersionOf: (self _nilOrClassNamed: aString)
  description: nil
  isModifiable: modifyBoolean
'.
			"2.4.5"
		#'subclass:instVarNames:classVars:classInstVars:poolDictionaries:inDictionary:constraints:instancesInvariant:isModifiable:' -> 'subclass: aString
instVarNames: anArrayOfStrings
classVars: anArrayOfClassVars
classInstVars: anArrayOfClassInstVars
poolDictionaries: anArrayOfPoolDicts
inDictionary: aDictionary
constraints: aConstraint
instancesInvariant: invarBoolean
isModifiable: modifyBoolean

"Obsolete in 2.1  The perferred methods are in the category Subclass Creation;
 see the similar method without the constraints keyword."

"Creates and returns a new subclass of the receiver.

 If aString is the name of a Class that is visible to the current user, this
 method creates the new class as a new version of the existing class, and they
 then share the same class history.  However, if no class named aString is
 visible to the user, then the new class is no relation to any existing class,
 and it has a new class history."

| oldClass desc |
self _validatePrivilege.
oldClass := self _nilOrClassNamed: aString .
desc := oldClass == nil ifTrue:[nil] ifFalse:[ oldClass description].
^self
  subclass: aString
  instVarNames: anArrayOfStrings
  classVars: anArrayOfClassVars
  classInstVars: anArrayOfClassInstVars
  poolDictionaries: anArrayOfPoolDicts
  inDictionary: aDictionary
  constraints: aConstraint
  instancesInvariant: invarBoolean
  newVersionOf: oldClass
  description: desc
  isModifiable: modifyBoolean
'.
			"2.3.1"
		#'indexableSubclass:instVarNames:classVars:classInstVars:poolDictionaries:inDictionary:instancesInvariant:newVersionOf:isModifiable:' -> 'indexableSubclass: aString
instVarNames: anArrayOfStrings
classVars: anArrayOfClassVars
classInstVars: anArrayOfClassInstVars
poolDictionaries: anArrayOfPoolDicts
inDictionary: aDictionary
instancesInvariant: invarBoolean
newVersionOf: oldClass
isModifiable: modifyBoolean

"Creates and returns a new indexable subclass of the receiver.  Instances of the
 new class are represented as pointer objects.

 If oldClass is visible to the current user, this method creates the new class
 as a new version of oldClass, and the two classes then share the same class
 history.  However, if oldClass is nil, then the new class is no relation
 to any existing class, and it has a new class history.

 This method generates an error if instances of the receiver are of special
 storage format or if they are NSCs."

self _validatePrivilege.
^ self indexableSubclass: aString
    instVarNames: anArrayOfStrings
    classVars: anArrayOfClassVars
    classInstVars: anArrayOfClassInstVars
    poolDictionaries: anArrayOfPoolDicts
    inDictionary: aDictionary
    constraints: #()
    instancesInvariant: invarBoolean
    newVersionOf: oldClass
    description: nil
    isModifiable: modifyBoolean
'.
			"2.4.5"
		#'indexableSubclass:instVarNames:classVars:classInstVars:poolDictionaries:inDictionary:instancesInvariant:newVersionOf:isModifiable:' -> 'indexableSubclass: aString
instVarNames: anArrayOfStrings
classVars: anArrayOfClassVars
classInstVars: anArrayOfClassInstVars
poolDictionaries: anArrayOfPoolDicts
inDictionary: aDictionary
instancesInvariant: invarBoolean
newVersionOf: oldClass
isModifiable: modifyBoolean

"Creates and returns a new indexable subclass of the receiver.  Instances of the
 new class are represented as pointer objects.

 If oldClass is visible to the current user, this method creates the new class
 as a new version of oldClass, and the two classes then share the same class
 history.  However, if oldClass is nil, then the new class is no relation
 to any existing class, and it has a new class history.

 This method generates an error if instances of the receiver are of special
 storage format or if they are NSCs."

self _validatePrivilege.
^ self indexableSubclass: aString
    instVarNames: anArrayOfStrings
    classVars: anArrayOfClassVars
    classInstVars: anArrayOfClassInstVars
    poolDictionaries: anArrayOfPoolDicts
    inDictionary: aDictionary
    constraints: #()
    instancesInvariant: invarBoolean
    newVersionOf: oldClass
    description: (oldClass isNil ifTrue: [nil] ifFalse: [oldClass description])
    isModifiable: modifyBoolean
'.
			"2.3.1"
		#'indexableSubclass:instVarNames:classVars:classInstVars:poolDictionaries:inDictionary:constraints:instancesInvariant:isModifiable:' -> 'indexableSubclass: aString
instVarNames: anArrayOfStrings
classVars: anArrayOfClassVars
classInstVars: anArrayOfClassInstVars
poolDictionaries: anArrayOfPoolDicts
inDictionary: aDictionary
constraints: aConstraint
instancesInvariant: invarBoolean
isModifiable: modifyBoolean

"Obsolete in 2.1.  The preferred methods are in the Subclass Creation
 category.  See the similar method without the constraints keyword."

"Creates and returns a new indexable subclass of the receiver.  Instances of the
 new class are represented as pointer objects.

 If aString is the name of a Class that is visible to the current user, this
 method creates the new class as a new version of the existing class, and they
 then share the same class history.  However, if no class named aString is
 visible to the user, then the new class is no relation to any existing class,
 and it has a new class history.

 This method generates an error if instances of the receiver are of special
 storage format or if they are NSCs."

self _validatePrivilege.
^self
  indexableSubclass: aString
  instVarNames: anArrayOfStrings
  classVars: anArrayOfClassVars
  classInstVars: anArrayOfClassInstVars
  poolDictionaries: anArrayOfPoolDicts
  inDictionary: aDictionary
  constraints: aConstraint
  instancesInvariant: invarBoolean
  newVersionOf: (self _nilOrClassNamed: aString)
  description: nil
  isModifiable: modifyBoolean
'.
			"2.4.5"
		#'indexableSubclass:instVarNames:classVars:classInstVars:poolDictionaries:inDictionary:constraints:instancesInvariant:isModifiable:' -> 'indexableSubclass: aString
instVarNames: anArrayOfStrings
classVars: anArrayOfClassVars
classInstVars: anArrayOfClassInstVars
poolDictionaries: anArrayOfPoolDicts
inDictionary: aDictionary
constraints: aConstraint
instancesInvariant: invarBoolean
isModifiable: modifyBoolean

"Obsolete in 2.1.  The preferred methods are in the Subclass Creation
 category.  See the similar method without the constraints keyword."

"Creates and returns a new indexable subclass of the receiver.  Instances of the
 new class are represented as pointer objects.

 If aString is the name of a Class that is visible to the current user, this
 method creates the new class as a new version of the existing class, and they
 then share the same class history.  However, if no class named aString is
 visible to the user, then the new class is no relation to any existing class,
 and it has a new class history.

 This method generates an error if instances of the receiver are of special
 storage format or if they are NSCs."

| oldClass desc |
self _validatePrivilege.
oldClass := self _nilOrClassNamed: aString .
desc := oldClass == nil ifTrue:[nil] ifFalse:[ oldClass description].
^self
  indexableSubclass: aString
  instVarNames: anArrayOfStrings
  classVars: anArrayOfClassVars
  classInstVars: anArrayOfClassInstVars
  poolDictionaries: anArrayOfPoolDicts
  inDictionary: aDictionary
  constraints: aConstraint
  instancesInvariant: invarBoolean
  newVersionOf: oldClass
  description: desc
  isModifiable: modifyBoolean
'.
			"2.3.1"
		#'byteSubclass:classVars:classInstVars:poolDictionaries:inDictionary:newVersionOf:instancesInvariant:' -> 'byteSubclass: aString
classVars: anArrayOfClassVars
classInstVars: anArrayOfClassInstVars
poolDictionaries: anArrayOfPoolDicts
inDictionary: aDictionary
newVersionOf: oldClass
instancesInvariant: invarBoolean

"Creates and returns a new byte subclass of the receiver.  You are not
 permitted to modify the new class after it is created.  If the receiver is not
 some kind of String class, then instances of the new class store and return
 SmallIntegers in the range 0 - 255.

 If oldClass is visible to the current user, this method creates the new class
 as a new version of oldClass, and the two classes then share the same class
 history.  However, if oldClass is nil, then the new class is no relation
 to any existing class, and it has a new class history.

 This method generates an error if instances of the receiver are of special
 storage format, if they are NSCs, or if they have instance variables."

self _validatePrivilege.
^ self byteSubclass: aString
    classVars: anArrayOfClassVars
    classInstVars: anArrayOfClassInstVars
    poolDictionaries: anArrayOfPoolDicts
    inDictionary: aDictionary
    newVersionOf: oldClass
    description: nil
    isInvariant: invarBoolean
'.
			"2.4.5"
		#'byteSubclass:classVars:classInstVars:poolDictionaries:inDictionary:newVersionOf:instancesInvariant:' -> 'byteSubclass: aString
classVars: anArrayOfClassVars
classInstVars: anArrayOfClassInstVars
poolDictionaries: anArrayOfPoolDicts
inDictionary: aDictionary
newVersionOf: oldClass
instancesInvariant: invarBoolean

"Creates and returns a new byte subclass of the receiver.  You are not
 permitted to modify the new class after it is created.  If the receiver is not
 some kind of String class, then instances of the new class store and return
 SmallIntegers in the range 0 - 255.

 If oldClass is visible to the current user, this method creates the new class
 as a new version of oldClass, and the two classes then share the same class
 history.  However, if oldClass is nil, then the new class is no relation
 to any existing class, and it has a new class history.

 This method generates an error if instances of the receiver are of special
 storage format, if they are NSCs, or if they have instance variables."

self _validatePrivilege.
^ self byteSubclass: aString
    classVars: anArrayOfClassVars
    classInstVars: anArrayOfClassInstVars
    poolDictionaries: anArrayOfPoolDicts
    inDictionary: aDictionary
    newVersionOf: oldClass
    description: (oldClass isNil ifTrue: [nil] ifFalse: [oldClass description])
    isInvariant: invarBoolean
'.
	}