! Enable SessionMethods, so that base class extensions are isolated. 
! Install Bootstrap and Monticello into global named BootstrapSymbolDictionaryName
run
| policy package targetSymbolDictionary |
package := GsPackageLibrary createPackageNamed: #SessionMethods.
GsPackageLibrary installPackage: package.
policy := GsPackagePolicy current.
targetSymbolDictionary := GsCurrentSession currentSession objectNamed: BootstrapSymbolDictionaryName.
(targetSymbolDictionary isKindOf: SymbolDictionary)
  ifFalse: [nil error: 'BootstrapSymbolDictionaryName must be the name of a SymbolDictionary'].
policy homeSymbolDict:  targetSymbolDictionary.
policy externalSymbolList: Array new.
true
%

!------------- SharedPool ------------------
expectvalue %String
run
| oldCls newCls |
oldCls := (System myUserProfile symbolList objectNamed: BootstrapSymbolDictionaryName) at:#SharedPool otherwise: nil .
oldCls == nil ifTrue:[
Object subclass: 'SharedPool'
  instVarNames: #()
  classVars: #()
  classInstVars: #()
  poolDictionaries: #[]
  category: 'Bootstrap-Squeak'
  inDictionary: (System myUserProfile symbolList objectNamed: BootstrapSymbolDictionaryName)
  constraints: #[  ]
  instancesInvariant: false
  isModifiable: false.
newCls := ((System myUserProfile symbolList objectNamed: BootstrapSymbolDictionaryName) at:#SharedPool) .
  ^ 'created new class: ' + newCls definition
 ]
ifFalse:[
  ^ 'existing class: ' + oldCls definition
 ]
%

!------------- Class ------------------
category: '*bootstrap'
method: Class
finalizeCreation
    self isInvariant ifFalse: [ self immediateInvariant ].
%
category: '*bootstrap'
method: Class
commentStamp: aStamp prior: anInt

	self finalizeCreation.
	^super commentStamp: aStamp prior: anInt
%
category: '*bootstrap'
method: Class
methodsFor: categoryName stamp: aString 

	self finalizeCreation.
	^ super methodsFor: categoryName stamp: aString
%
category: '*bootstrap'
method: Class
poolDictionariesForNames: poolDictionaryNames

	| ar existingDict |
	ar := Array new.
	poolDictionaryNames do: [:poolName |
		existingDict := GsSession currentSession userProfile symbolList objectNamed: poolName.
		(existingDict isKindOf: SymbolDictionary)
			ifTrue: [ ar add: existingDict ].
		((existingDict isKindOf: Class) and: [existingDict isSubclassOf: SharedPool])
			ifTrue: [ 
				ar add: existingDict _classVars.
				existingDict _classVars name: poolName asSymbol.
			].
		existingDict == nil
			ifTrue: [ | pool |
				pool := SymbolDictionary new.
				pool name: poolName asSymbol.
				ar add: pool.
			].
	].
^ar
%
category: '*bootstrap'
method: Class
variableByteSubclass: subclassName
	instanceVariableNames: instanceVariableNames
	classVariableNames: classVariableNames
	poolDictionaries: poolDictionaryNames
	category: category

  | cl |
  cl := self byteSubclass: subclassName
	classVars: (classVariableNames findTokens: ' ')  asArray
	classInstVars: #()
	poolDictionaries: (self poolDictionariesForNames: (poolDictionaryNames findTokens: ' ') asArray)
	category: category
	inDictionary: GsPackagePolicy current homeSymbolDict
	instancesInvariant: false
	isModifiable: true.
  self _resolveUndeclaredSymbolsFor: cl.
  ^cl
%
category: '*bootstrap'
method: Class
variableSubclass: subclassName
	instanceVariableNames: instanceVariableNames
	classVariableNames: classVariableNames
	poolDictionaries: poolDictionaryNames
	category: category

  | cl |
  cl := self indexableSubclass: subclassName
	instVarNames: (instanceVariableNames findTokens: ' ') asArray
	classVars: (classVariableNames findTokens: ' ')  asArray
	classInstVars: #()
	poolDictionaries: (self poolDictionariesForNames: (poolDictionaryNames findTokens: ' ') asArray)
	category: category
	inDictionary: GsPackagePolicy current homeSymbolDict
	instancesInvariant: false
	isModifiable: true.
  self _resolveUndeclaredSymbolsFor: cl.
  ^cl
%
category: '*bootstrap'
method: Class
transientSubclass: subclassName
	instanceVariableNames: instanceVariableNames
	classVariableNames: classVariableNames
	poolDictionaries: poolDictionaryNames
	category: category

| cl |
cl := self subclass: subclassName
	instanceVariableNames: instanceVariableNames
	classVariableNames: classVariableNames
	poolDictionaries: poolDictionaryNames
	category: category.
cl makeInstancesDbTransient.
^cl
%
category: '*bootstrap'
method: Class
subclass: subclassName
	instanceVariableNames: instanceVariableNames
	classVariableNames: classVariableNames
	poolDictionaries: poolDictionaryNames
	category: category

  | cl |
  cl := self subclass: subclassName
	instVarNames: (instanceVariableNames findTokens: ' ') asArray
	classVars: (classVariableNames findTokens: ' ')  asArray
	classInstVars: #()
	poolDictionaries: (self poolDictionariesForNames: (poolDictionaryNames findTokens: ' ') asArray)
	category: category
	inDictionary: GsPackagePolicy current homeSymbolDict
	constraints: #[]
	instancesInvariant: false
	isModifiable: true.
  self _resolveUndeclaredSymbolsFor: cl.
  ^cl
%
category: '*bootstrap'
method: Class
_resolveUndeclaredSymbolsFor: cl
  | undefinedSymbols |
  undefinedSymbols := GsSession currentSession symbolList objectNamed: #UndefinedSymbols.
  undefinedSymbols ~~ nil
    ifTrue: [
      (undefinedSymbols at: cl name asSymbol otherwise: Set new) do: [:assoc | | cl selector stamp |
         cl := assoc key.
         selector := assoc value.
         stamp := cl stampForMethod: selector.
         cl
            compileMethod: (cl compiledMethodAt: selector) sourceString
            category: (cl categoryOfSelector: selector).
         cl setStamp: stamp forMethod: selector ].
      undefinedSymbols removeKey: cl name asSymbol ifAbsent: [].
    ].
%
category: '*bootstrap'
method: Class
byteSubclass: aString
classVars: anArrayOfClassVars
classInstVars: anArrayOfClassInstVars
poolDictionaries: anArrayOfPoolDicts
category: aCategoryName
inDictionary: aDictionary
instancesInvariant: invarBoolean
isModifiable: modifyBoolean

| cl |
cl := self 
	byteSubclass: aString
	classVars: anArrayOfClassVars
	classInstVars: anArrayOfClassInstVars
	poolDictionaries: anArrayOfPoolDicts
	inDictionary: aDictionary
	instancesInvariant: invarBoolean
    isModifiable: modifyBoolean.
cl category: aCategoryName asString.
^cl
%
category: '*bootstrap'
method: Class
byteSubclass: aString
classVars: anArrayOfClassVars
classInstVars: anArrayOfClassInstVars
poolDictionaries: anArrayOfPoolDicts
inDictionary: aDictionary
instancesInvariant: invarBoolean
isModifiable: modifyBoolean

"Creates and returns a new byte subclass of the receiver.  You are not
 permitted to modify the new class after it is created.  If the receiver is not
 some kind of String class, then instances of the new class store and return
 SmallIntegers in the range 0 - 255.

 If aString is the name of a Class that is visible to the current user, this
 method creates the new class as a new version of the existing class, and they
 then share the same class history.  However, if no class named aString is
 visible to the user, then the new class is no relation to any existing class,
 and it has a new class history.

 This method generates an error if instances of the receiver are of special
 storage format, if they are NSCs, or if they have instance variables."

self _validatePrivilege.
^self
  byteSubclass: aString
  classVars: anArrayOfClassVars
  classInstVars: anArrayOfClassInstVars
  poolDictionaries: anArrayOfPoolDicts
  inDictionary: aDictionary
  newVersionOf: (self _nilOrClassNamed: aString)
  description: nil
  isInvariant: invarBoolean
  isModifiable: modifyBoolean
%
category: '*base-bootstrap'
method: Class
byteSubclass: aString
classVars: anArrayOfClassVars
classInstVars: anArrayOfClassInstVars
poolDictionaries: anArrayOfPoolDicts
inDictionary: aDictionary
newVersionOf: oldClass
description: aDescription
isInvariant: invarBoolean
isModifiable: modifyBoolean

"Obsolete in GemStone 4.1.  The preferred methods are in the Subclass Creation
 category.  Look for the method that omits this method's keyword description:
 and changes its keyword isInvariant: to instancesInvariant:."

| hist theFormat |
self _validatePrivilege.
oldClass ~~ nil ifTrue:[  "fix bug 11833"
  oldClass _validateClass: Class .
  hist := oldClass classHistory .
  ].

(self subclassesDisallowed) ifTrue: [
  ^ self _error: #classErrSubclassDisallowed].
aDictionary _validateClass: SymbolDictionary.
(instVars ~~ 0) ifTrue: [^ self _error: #classErrByteObjInstVars].
((self instancesInvariant) & (invarBoolean not)) ifTrue: [

  ^ self _error: #classErrInvariantSuperClass
  ].
(self isNsc) ifTrue: [ ^ aString _error: #classErrBadFormat ].
theFormat := (format bitAnd: 16r3 bitInvert) bitOr: (16r1 + 16r4) . 
invarBoolean ifTrue:[ theFormat := theFormat bitOr: 16r8 ].

^ self _subclass: aString
         instVarNames:  #() 
         format: theFormat
         constraints:  #() 
         classVars: anArrayOfClassVars
         classInstVars: anArrayOfClassInstVars
         poolDictionaries: anArrayOfPoolDicts
         inDictionary: aDictionary
         inClassHistory: hist
         description: aDescription
         isModifiable: modifyBoolean
%
category: '*bootstrap'
method: Class
_description: aDescription
    "Update the description of this Class.  Returns the argument."
    self commentStamp: self changeStamp.
    ^ self description: aDescription
%
category: '*bootstrap'
method: Class
_subclass: className
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

"undo the compiler's canonicalization of empty arrays (fix bug 14103) "
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

theHist := aClassHistory .
theHist == nil
  ifTrue: [ theHist := ClassHistory new name: className ] .
theHist add: result .
result classHistory: theHist .
result timeStamp: DateTime now.
result userId: System myUserProfile userId.
result extraDict: SymbolDictionary new .
result _description: aDescription .

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
%
category: '*base-bootstrap'
method: Class
_definitionInContext: aUserProfile

"Returns a description of the receiver using object names taken from the given
 UserProfile."

| result newByteSubclass anArray lfsp
  aConstraint firstElement inv civs |

result := String new.
result addAll: (superClass == nil ifTrue: ['nil'] ifFalse: [superClass name]).

newByteSubclass := false.
(lfsp := Character lf asString) addAll: '  ' .


(self isBytes _and: [superClass isBytes not]) ifTrue: [
  result addAll: ' byteSubclass: '''; addAll: name; addLast: $'.
  newByteSubclass := true.
]
ifFalse: [
  (self isIndexable _and: [superClass isIndexable not]) ifTrue: [
    result addAll: ' indexableSubclass: '''; addAll: name; addLast: $'.
  ]
  ifFalse: [
    result addAll: ' subclass: '''; addAll: name; addLast: $'.
  ]
].

" instVarNames: #( <list of strings> ) "

newByteSubclass ifFalse: [
  result addAll: lfsp;
    addAll: 'instVarNames: #(';
    addAll: (self _instVarNamesWithSeparator: (lfsp , '                 '));
    add: $).
].

" classVars: #( <list of strings> ) "
result addAll: lfsp; addLast: 'classVars: #('.
self _sortedClassVarNames do: [:aKey |
  result addLast: $  . 
  (aKey includesValue: $') 
    ifTrue:[ result addAll: aKey _asSource ]
    ifFalse:[ result addAll: aKey ].
  ].
result addLast: $).

" classInstVars: #( <list of strings> ) "

result addAll: lfsp; addLast: 'classInstVars: #('.
civs := self class allInstVarNames.
civs removeFrom: 1 to: (self class superClass instSize).
civs do: [:civName |
  result addLast: $  .
  (civName includesValue: $') 
    ifTrue:[ result addAll: civName _asSource ]
    ifFalse:[ result addAll: civName ].
].
result addLast: $).

" poolDictionaries: #[ <list of dictionary names> ] "

inv := poolDictionaries ~~ nil and:[poolDictionaries isKindOf: InvariantArray] .
result addAll: lfsp; addAll: 'poolDictionaries: #'.

result add: (inv ifTrue: [ $( ] ifFalse: [ $[ ]).

firstElement := true.
self sharedPools do: [:each |
  firstElement ifFalse: [ result addAll: ', '].  "separate with commas"
  anArray := aUserProfile dictionaryAndSymbolOf: each.
  anArray == nil
        ifTrue: [ | d |
            (d := aUserProfile objectNamed: each name) ~~ nil 
			ifTrue: [ 
				((d isKindOf: Class) and: [d isSubclassOf: SharedPool])
					ifTrue: [ result addAll: ( each name, ' _classVars')  ]
					ifFalse: [ result addAll: ' "(not named)" ' ]
			]
			ifFalse: [result addAll: ' "(not named)" ' ]
        ]
        ifFalse: [ result addAll: (anArray at: 2)].
  firstElement := false.
].

result add: (inv ifTrue: [ $) ] ifFalse: [ $] ]).

"category: <name of class category>"

classCategory ~~ nil
    ifTrue: [
        result addAll: lfsp; addAll: 'category: '.
        result addAll: classCategory printString.
    ].

" inDictionary: <name of containing dictionary> "

result addAll: lfsp; addAll: 'inDictionary: '.
anArray := aUserProfile dictionaryAndSymbolOf: self.
anArray == nil ifTrue: [
  result addAll: '(class not in your dictionaries)'
]
ifFalse: [
  anArray := aUserProfile dictionaryAndSymbolOf: (anArray at: 1).
  anArray == nil ifTrue: [
    result addAll: '(dictionary not in your dictionaries)'
  ]
  ifFalse: [
    result addAll: (anArray at: 2)
  ]
].

" constraints: #[ <Array of instance-variable-symbol/class-name pairs> ]
    or
  constraints: <class name> "

newByteSubclass ifFalse: [
  result addAll: lfsp; addAll: 'constraints: '.
  (constraints _isKindOf: Array) ifTrue: [
    result addAll: '#[ '.
    firstElement := true.
    1 to: instVars do: [ :x |
      aConstraint := constraints at:x .
      ((aConstraint ~~ nil _and: [aConstraint ~~ Object]) 
          _and:[ superClass == nil 
            _or:[ (superClass _namedIvConstraintAt: x) ~~ aConstraint ]] )
      ifTrue: [
        " if not the first constraint, prefix with a comma to separate
          from the last constraint "
        firstElement ifFalse: [
          result addLast: $,; addAll: lfsp; addAll: '                '
        ]
        ifTrue: [
          firstElement := false
        ].
        result addAll: '#[ #'; addAll: (instVarNames at: x) ;
              addAll: ', '; addAll: aConstraint name; addLast: $] .
      ]
    ].
    aConstraint:= self varyingConstraint .
    ( (aConstraint ~~ Object) _and:
        [(superClass varyingConstraint) ~~ aConstraint] )
    ifTrue:[
      firstElement ifFalse: [
          result addLast: $,; addAll: lfsp; addAll: '                '
      ]
      ifTrue: [
        firstElement := false
      ].
      result addAll: '   "the elements"  '; addAll: aConstraint name
    ].
    result addAll: ' ]'.
  ]
  ifFalse: [
    constraints class class == Metaclass ifTrue: [
      result addAll: constraints name.
    ]
    ifFalse: [
      result addAll: ' nil'
    ].
  ].

    " instancesInvariant: "

  result addAll: lfsp;
    addAll: 'instancesInvariant: ';
    addAll: (self instancesInvariant describe).

    " instancesInvariant: aBoolean "

  result addAll: lfsp;
    addAll: 'isModifiable: '; addAll: (self isModifiable describe).
]
ifTrue: [ "a Byte subclass"

    " instancesInvariant: aBoolean "                           "fix 9763"

  result addAll: lfsp;
    addAll: 'instancesInvariant: '; addAll: (self instancesInvariant describe).
].

result add: Character lf.
^result
%
!------------- 
category: '*bootstrap-subclass-creation'
classmethod: Class
_defineClassNamed: aClassName with: aBlock
	| originalClass classOrganizer cls |
	originalClass := System myUserProfile symbolList objectNamed: aClassName asSymbol.
	originalClass ~~ nil 
		ifTrue: [ 
			"pre-create the classOrganizer, so that original subclasses are preserved."
			classOrganizer := ClassOrganizer cachedOrganizer ].
	cls := aBlock value.
	cls == nil
		ifTrue: [ ^nil ].
	originalClass ~~ nil
		ifTrue: [
			cls 
				copyMethodsFrom: originalClass 
				dictionaries: GsSession currentSession symbolList ].
	Class _resolveUndeclaredSymbolsForClass: cls.
	^cls
%
category: '*bootstrap-caching'
classmethod: ClassOrganizer
clearCachedOrganizer

%
category: '*bootstrap-caching'
classmethod: ClassOrganizer
cachedOrganizer

    ^self new
%
category: '*base-bootstrap'
method: Behavior
compileMethod: source category: cat using: aSymbolList 
"This method will be overridden if compilation notification code is loaded"

    ^self _compileMethod: source category: cat using: aSymbolList 
%

