category: '*core-squeak-subclass creation'
method: Class
indexableSubclass: aString
instVarNames: anArrayOfStrings
classVars: anArrayOfClassVars
classInstVars: anArrayOfClassInstVars
poolDictionaries: anArrayOfPoolDicts
inDictionary: aDictionaryName
category: aCategoryName

	^Class _defineClassNamed: aString with: [ | symbolDict |
		symbolDict := aDictionaryName isEmpty
			ifTrue: [GsPackagePolicy current homeSymbolDict]
			ifFalse: [System myUserProfile symbolList objectNamed: aDictionaryName asSymbol].
		self indexableSubclass: aString
			instVarNames: anArrayOfStrings
			classVars: anArrayOfClassVars
			classInstVars: anArrayOfClassInstVars
			poolDictionaries: anArrayOfPoolDicts
			category: aCategoryName
			inDictionary: symbolDict
			instancesInvariant: false
			isModifiable: false ]
%
category: '*core-squeak-subclass creation'
method: Class
subclass: aString
instVarNames: anArrayOfStrings
classVars: anArrayOfClassVars
classInstVars: anArrayOfClassInstVars
poolDictionaries: anArrayOfPoolDicts
inDictionary: aDictionaryName
category: aCategoryName

	^Class _defineClassNamed: aString with: [ | symbolDict |
		symbolDict := aDictionaryName isEmpty
			ifTrue: [GsPackagePolicy current homeSymbolDict]
			ifFalse: [System myUserProfile symbolList objectNamed: aDictionaryName asSymbol].
		self subclass: aString
			instVarNames: anArrayOfStrings
			classVars: anArrayOfClassVars
			classInstVars: anArrayOfClassInstVars
			poolDictionaries: anArrayOfPoolDicts
			category: aCategoryName
			inDictionary: symbolDict
			constraints: #[]
			instancesInvariant: false
			isModifiable: false].
%
category: '*core-squeak-subclass creation'
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
category: '*core-squeak-subclass creation'
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

"clear the ClassOrganizer cache"
ClassOrganizer clearCachedOrganizer.

^ result
%
category: '*core-squeak-subclass creation'
method: Class
byteSubclass: aString
classVars: anArrayOfClassVars
classInstVars: anArrayOfClassInstVars
poolDictionaries: anArrayOfPoolDicts
category: aCategoryName

	^self byteSubclass: aString
		classVars: anArrayOfClassVars
		classInstVars: anArrayOfClassInstVars
		poolDictionaries: anArrayOfPoolDicts
		inDictionary: ''
		category: aCategoryName
%
category: '*core-squeak-subclass creation'
method: Class
_resolveUndeclaredSymbolsFor: aSymbol
	| undefinedSymbols |
	undefinedSymbols := GsSession currentSession symbolList objectNamed: #UndefinedSymbols.
	undefinedSymbols ~~ nil
		ifTrue: [
			(undefinedSymbols at: aSymbol otherwise: Set new) do: [:assoc | 
				(assoc key includesSelector: assoc value) 
					ifTrue: [ assoc key _recompile: assoc value ]].
			(undefinedSymbols at: aSymbol otherwise: Set new) isEmpty 
				ifTrue: [ undefinedSymbols removeKey: aSymbol ifAbsent: []]].
%
category: '*core-squeak-subclass creation'
method: Class
indexableSubclass: aString
instVarNames: anArrayOfStrings
classVars: anArrayOfClassVars
classInstVars: anArrayOfClassInstVars
poolDictionaries: anArrayOfPoolDicts
category: aCategoryName

	^self indexableSubclass: aString
		instVarNames: anArrayOfStrings
		classVars: anArrayOfClassVars
		classInstVars: anArrayOfClassInstVars
		poolDictionaries: anArrayOfPoolDicts
		inDictionary: ''
		category: aCategoryName
%
category: '*core-squeak-subclass creation'
method: Class
variableByteSubclass: subclassName
	instanceVariableNames: instanceVariableNames
	classVariableNames: classVariableNames
	poolDictionaries: poolDictionaryNames
	category: category

  ^self byteSubclass: subclassName
	classVars: (classVariableNames findTokens: ' ')  asArray
	classInstVars: #()
	poolDictionaries: (self poolDictionariesForNames: (poolDictionaryNames findTokens: ' ') asArray)
	inDictionary: ''
	category: category
%
category: '*core-squeak-subclass creation'
method: Class
subclass: aString
instVarNames: anArrayOfStrings
classVars: anArrayOfClassVars
classInstVars: anArrayOfClassInstVars
poolDictionaries: anArrayOfPoolDicts
category: aCategoryName

	^self subclass: aString
		instVarNames: anArrayOfStrings
		classVars: anArrayOfClassVars
		classInstVars: anArrayOfClassInstVars
		poolDictionaries: anArrayOfPoolDicts
		inDictionary: ''
		category: aCategoryName
%
category: '*core-squeak-subclass creation'
method: Class
transientSubclass: aString
instVarNames: anArrayOfStrings
classVars: anArrayOfClassVars
classInstVars: anArrayOfClassInstVars
poolDictionaries: anArrayOfPoolDicts
category: aCategoryName

	^self transientSubclass: aString
		instVarNames: anArrayOfStrings
		classVars: anArrayOfClassVars
		classInstVars: anArrayOfClassInstVars
		poolDictionaries: anArrayOfPoolDicts
		inDictionary: ''
		category: aCategoryName
%
category: '*core-squeak-subclass creation'
method: Class
transientSubclass: subclassName
	instanceVariableNames: instanceVariableNames
	classVariableNames: classVariableNames
	poolDictionaries: poolDictionaryNames
	category: category

	^self transientSubclass: subclassName
		instVarNames: (instanceVariableNames findTokens: ' ') asArray
		classVars: (classVariableNames findTokens: ' ')  asArray
		classInstVars: #()
		poolDictionaries: (self poolDictionariesForNames: (poolDictionaryNames findTokens: ' ') asArray)
		inDictionary: ''
		category: category
%
category: '*core-squeak-subclass creation'
method: Class
variableSubclass: subclassName
	instanceVariableNames: instanceVariableNames
	classVariableNames: classVariableNames
	poolDictionaries: poolDictionaryNames
	category: category

	^self indexableSubclass: subclassName
		instVarNames: (instanceVariableNames findTokens: ' ') asArray
		classVars: (classVariableNames findTokens: ' ')  asArray
		classInstVars: #()
		poolDictionaries: (self poolDictionariesForNames: (poolDictionaryNames findTokens: ' ') asArray)
		inDictionary: ''
		category: category
%
category: '*core-squeak-subclass creation'
method: Class
transientSubclass: aString
instVarNames: anArrayOfStrings
classVars: anArrayOfClassVars
classInstVars: anArrayOfClassInstVars
poolDictionaries: anArrayOfPoolDicts
category: aCategoryName
inDictionary: aDictionary
constraints: aConstraint
instancesInvariant: invarBoolean
isModifiable: modifyBoolean

| cl |
cl := self subclass: aString
	instVarNames: anArrayOfStrings
	classVars: anArrayOfClassVars
	classInstVars: anArrayOfClassInstVars
	poolDictionaries: anArrayOfPoolDicts
	inDictionary: aDictionary
	constraints: aConstraint
	instancesInvariant: invarBoolean
	isModifiable: true.
cl category: aCategoryName asString.
cl makeInstancesDbTransient.
modifyBoolean ifFalse:[ cl immediateInvariant ] .
^cl
%
category: '*core-squeak-subclass creation'
method: Class
byteSubclass: aString
classVars: anArrayOfClassVars
classInstVars: anArrayOfClassInstVars
poolDictionaries: anArrayOfPoolDicts
inDictionary: aDictionaryName
category: aCategoryName

	^Class _defineClassNamed: aString with: [ | symbolDict |
		symbolDict := aDictionaryName isEmpty
			ifTrue: [GsPackagePolicy current homeSymbolDict]
			ifFalse: [System myUserProfile symbolList objectNamed: aDictionaryName asSymbol].
		self byteSubclass: aString
			classVars: anArrayOfClassVars
			classInstVars: anArrayOfClassInstVars
			poolDictionaries: anArrayOfPoolDicts
			category: aCategoryName
			inDictionary: symbolDict
			instancesInvariant: false
			isModifiable: false ]
%
category: '*core-squeak-subclass creation'
method: Class
subclass: subclassName
	instanceVariableNames: instanceVariableNames
	classVariableNames: classVariableNames
	poolDictionaries: poolDictionaryNames
	category: category

	^self subclass: subclassName
		instVarNames: (instanceVariableNames findTokens: ' ') asArray
		classVars: (classVariableNames findTokens: ' ')  asArray
		classInstVars: #()
		poolDictionaries: (self poolDictionariesForNames: (poolDictionaryNames findTokens: ' ') asArray)
		inDictionary: ''
		category: category
%
category: '*core-squeak-subclass creation'
method: Class
transientSubclass: aString
instVarNames: anArrayOfStrings
classVars: anArrayOfClassVars
classInstVars: anArrayOfClassInstVars
poolDictionaries: anArrayOfPoolDicts
inDictionary: aDictionaryName
category: aCategoryName

	^Class _defineClassNamed: aString with: [ | symbolDict |
		symbolDict := aDictionaryName isEmpty
			ifTrue: [GsPackagePolicy current homeSymbolDict]
			ifFalse: [System myUserProfile symbolList objectNamed: aDictionaryName asSymbol].
		self transientSubclass: aString
			instVarNames: anArrayOfStrings
			classVars: anArrayOfClassVars
			classInstVars: anArrayOfClassInstVars
			poolDictionaries: anArrayOfPoolDicts
			category: aCategoryName
			inDictionary: symbolDict
			constraints: #[]
			instancesInvariant: false
			isModifiable: false ]
%
category: '*core-squeak-subclass creation'
method: Class
_resolveUndeclaredSymbolsForClass: cl
	
	self _resolveUndeclaredSymbolsFor: cl name asSymbol
%
category: '*core-squeak-subclass creation'
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
doit
Object subclass: 'DiskProxy'
	instVarNames: #( globalObjectName preSelector constructorSelector
	                  constructorArgs)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #[]
	inDictionary: ''
	category: 'Core-Squeak'.
true
%
doit
Object subclass: 'ClassCategoryReader'
	instVarNames: #( class category changeStamp)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #[]
	inDictionary: ''
	category: 'Core-Gemstone'.
true
%
doit
ClassCategoryReader subclass: 'ClassCommentReader'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #[]
	inDictionary: ''
	category: 'Core-Gemstone'.
true
%
doit
SequenceableCollection subclass: 'ArrayedCollection'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #[]
	inDictionary: ''
	category: 'Core-SharedPool'.
true
%
doit
ArrayedCollection subclass: 'WordArray'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #[]
	inDictionary: ''
	category: 'Core-SharedPool'.
true
%
doit
Notification subclass: 'UndefinedSymbolNotification'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #[]
	inDictionary: ''
	category: 'Core-Gemstone'.
true
%
doit
Object subclass: 'SharedPool'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #[]
	inDictionary: ''
	category: 'Core-SharedPool'.
true
%
doit
SharedPool subclass: 'ZipConstants'
	instVarNames: #()
	classVars: #( BaseDistance BaseLength BitLengthOrder DistanceCodes DynamicBlock EndBlock ExtraBitLengthBits ExtraDistanceBits ExtraLengthBits FixedBlock FixedDistanceTree FixedLiteralTree HashBits HashMask HashShift MatchLengthCodes MaxBitLengthBits MaxBitLengthCodes MaxBits MaxDistance MaxDistCodes MaxLengthCodes MaxLiteralCodes MaxMatch MinMatch NumLiterals Repeat11To138 Repeat3To10 Repeat3To6 StoredBlock WindowMask WindowSize)
	classInstVars: #()
	poolDictionaries: #[]
	inDictionary: ''
	category: 'Core-SharedPool'.
true
%
doit
SharedPool subclass: 'ZipFileConstants'
	instVarNames: #()
	classVars: #( CentralDirectoryFileHeaderSignature CompressionDeflated CompressionLevelDefault CompressionLevelNone CompressionStored DataDescriptorLength DefaultDirectoryPermissions DefaultFilePermissions DeflatingCompressionFast DeflatingCompressionMaximum DeflatingCompressionNormal DeflatingCompressionSuperFast DirectoryAttrib EndOfCentralDirectorySignature FaMsdos FaUnix FileAttrib IfaBinaryFile IfaTextFile LocalFileHeaderSignature)
	classInstVars: #()
	poolDictionaries: #[]
	inDictionary: ''
	category: 'Core-SharedPool'.
true
%
doit
Object subclass: 'ZipEncoderNode'
	instVarNames: #( value frequency height
	                  bitLength code parent left
	                  right)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #[]
	inDictionary: ''
	category: 'Core-SharedPool'.
true
%
doit
Object subclass: 'ZipEncoderTree'
	instVarNames: #( bitLengths codes maxCode)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #[]
	inDictionary: ''
	category: 'Core-SharedPool'.
true
%

! Remove existing behavior from ClassCategoryReader
removeallmethods ClassCategoryReader
removeallclassmethods ClassCategoryReader
! ------------------- Class methods for ClassCategoryReader
! ------------------- Instance methods for ClassCategoryReader
category: 'fileIn/Out'
method: ClassCategoryReader
scanFrom: aStream 
	"File in methods from the stream, aStream."
	| methodText |
	[methodText := aStream nextChunk.
	 methodText size > 0]
		whileTrue:
		[ 
        class compileMethod: methodText withGemstoneLineEndings category: category.
		changeStamp ~~ nil 
          ifTrue: [class setStamp: changeStamp forMethod: (Behavior parseSelector: methodText for: class)]
		]
%
category: 'private'
method: ClassCategoryReader
setClass: aClass category: aCategory

	class := aClass.
	category := aCategory.
	changeStamp := nil
%
category: 'private'
method: ClassCategoryReader
setClass: aClass category: aCategory changeStamp: aString

	class := aClass.
	category := aCategory.
	changeStamp := aString
%

! Remove existing behavior from ClassCommentReader
removeallmethods ClassCommentReader
removeallclassmethods ClassCommentReader
! ------------------- Class methods for ClassCommentReader
! ------------------- Instance methods for ClassCommentReader
category: 'fileIn/Out'
method: ClassCommentReader
scanFrom: aStream
| doc txt |
doc := GsClassDocumentation newForClass: class.
txt := (GsDocText new) details: aStream nextChunk.
doc documentClassWith: txt.
class _description: doc.
changeStamp ~~ nil ifTrue: [ class commentStamp: changeStamp ]
%

! Remove existing behavior from ArrayedCollection
removeallmethods ArrayedCollection
removeallclassmethods ArrayedCollection
! ------------------- Class methods for ArrayedCollection
category: 'instance creation'
classmethod: ArrayedCollection
new
	"Answer a new instance of me, with size = 0."

	^self new: 0
%
category: 'instance creation'
classmethod: ArrayedCollection
new: size withAll: value 
	"Answer an instance of me, with number of elements equal to size, each 
	of which refers to the argument, value."

	^(self new: size) atAllPut: value
%
category: 'instance creation'
classmethod: ArrayedCollection
newFrom: aCollection 
	"Answer an instance of me containing the same elements as aCollection."
	| newArray |
	newArray _ self new: aCollection size.
	1 to: aCollection size do: [:i | newArray at: i put: (aCollection at: i)].
	^ newArray

"	Array newFrom: {1. 2. 3}
	{1. 2. 3} as: Array
	{1. 2. 3} as: ByteArray
	{$c. $h. $r} as: String
	{$c. $h. $r} as: Text
"
%
category: 'instance creation'
classmethod: ArrayedCollection
with: anObject 
	"Answer a new instance of me, containing only anObject."

	| newCollection |
	newCollection _ self new: 1.
	newCollection at: 1 put: anObject.
	^newCollection
%
category: 'instance creation'
classmethod: ArrayedCollection
with: firstObject with: secondObject 
	"Answer a new instance of me, containing firstObject and secondObject."

	| newCollection |
	newCollection _ self new: 2.
	newCollection at: 1 put: firstObject.
	newCollection at: 2 put: secondObject.
	^newCollection
%
category: 'instance creation'
classmethod: ArrayedCollection
with: firstObject with: secondObject with: thirdObject 
	"Answer a new instance of me, containing only the three arguments as
	elements."

	| newCollection |
	newCollection _ self new: 3.
	newCollection at: 1 put: firstObject.
	newCollection at: 2 put: secondObject.
	newCollection at: 3 put: thirdObject.
	^newCollection
%
category: 'instance creation'
classmethod: ArrayedCollection
with: firstObject with: secondObject with: thirdObject with: fourthObject 
	"Answer a new instance of me, containing only the three arguments as
	elements."

	| newCollection |
	newCollection _ self new: 4.
	newCollection at: 1 put: firstObject.
	newCollection at: 2 put: secondObject.
	newCollection at: 3 put: thirdObject.
	newCollection at: 4 put: fourthObject.
	^newCollection
%
category: 'instance creation'
classmethod: ArrayedCollection
with: firstObject with: secondObject with: thirdObject with: fourthObject with: fifthObject
	"Answer a new instance of me, containing only the five arguments as
	elements."

	| newCollection |
	newCollection _ self new: 5.
	newCollection at: 1 put: firstObject.
	newCollection at: 2 put: secondObject.
	newCollection at: 3 put: thirdObject.
	newCollection at: 4 put: fourthObject.
	newCollection at: 5 put: fifthObject.
	^newCollection
%
category: 'instance creation'
classmethod: ArrayedCollection
with: firstObject with: secondObject with: thirdObject with: fourthObject with: fifthObject with: sixthObject
	"Answer a new instance of me, containing only the 6 arguments as elements."

	| newCollection |
	newCollection _ self new: 6.
	newCollection at: 1 put: firstObject.
	newCollection at: 2 put: secondObject.
	newCollection at: 3 put: thirdObject.
	newCollection at: 4 put: fourthObject.
	newCollection at: 5 put: fifthObject.
	newCollection at: 6 put: sixthObject.
	^ newCollection
%
category: 'instance creation'
classmethod: ArrayedCollection
withAll: aCollection
	"Create a new collection containing all the elements from aCollection."

	^ (self new: aCollection size) replaceFrom: 1 to: aCollection size with: aCollection
%
! ------------------- Instance methods for ArrayedCollection
category: 'adding'
method: ArrayedCollection
add: newObject
	self shouldNotImplement
%
category: 'sorting'
method: ArrayedCollection
asSortedArray
	self isSorted ifTrue: [^ self asArray].
	^ super asSortedArray
%
category: 'objects from disk'
method: ArrayedCollection
byteSize
	^self basicSize * self bytesPerBasicElement
%
category: 'objects from disk'
method: ArrayedCollection
bytesPerBasicElement
	"Answer the number of bytes that each of my basic elements requires.
	In other words:
		self basicSize * self bytesPerBasicElement
	should equal the space required on disk by my variable sized representation."
	^self class isBytes ifTrue: [ 1 ] ifFalse: [ 4 ]
%
category: 'objects from disk'
method: ArrayedCollection
bytesPerElement
	^self class isBytes ifTrue: [ 1 ] ifFalse: [ 4 ].
%
category: 'private'
method: ArrayedCollection
defaultElement

	^nil
%
category: 'sorting'
method: ArrayedCollection
isSorted
	"Return true if the receiver is sorted by the given criterion.
	Optimization for isSortedBy: [:a :b | a <= b]."

	| lastElm elm |
	self isEmpty ifTrue: [^ true].
	lastElm _ self first.
	2 to: self size do: 
		[:index | 
		elm _ self at: index.
		lastElm <= elm ifFalse: [^ false].
		lastElm _ elm].
	^ true
%
category: 'sorting'
method: ArrayedCollection
isSortedBy: aBlock
	"Return true if the receiver is sorted by the given criterion."

	| lastElm elm |
	self isEmpty ifTrue: [^ true].
	lastElm _ self first.
	2 to: self size do: 
		[:index | 
		elm _ self at: index.
		(aBlock value: lastElm value: elm) ifFalse: [^ false].
		lastElm _ elm].
	^ true
%
category: 'private'
method: ArrayedCollection
storeElementsFrom: firstIndex to: lastIndex on: aStream

	| noneYet defaultElement arrayElement |
	noneYet _ true.
	defaultElement _ self defaultElement.
	firstIndex to: lastIndex do: 
		[:index | 
		arrayElement _ self at: index.
		arrayElement = defaultElement
			ifFalse: 
				[noneYet
					ifTrue: [noneYet _ false]
					ifFalse: [aStream nextPut: $;].
				aStream nextPutAll: ' at: '.
				aStream store: index.
				aStream nextPutAll: ' put: '.
				aStream store: arrayElement]].
	^noneYet
%
category: 'printing'
method: ArrayedCollection
storeOn: aStream

	aStream nextPutAll: '(('.
	aStream nextPutAll: self class name.
	aStream nextPutAll: ' new: '.
	aStream store: self size.
	aStream nextPut: $).
	(self storeElementsFrom: 1 to: self size on: aStream)
		ifFalse: [aStream nextPutAll: '; yourself'].
	aStream nextPut: $)
%

! Remove existing behavior from WordArray
removeallmethods WordArray
removeallclassmethods WordArray
! ------------------- Class methods for WordArray
category: 'plugin generation'
classmethod: WordArray
ccgDeclareCForVar: aSymbolOrString

	^'unsigned *', aSymbolOrString
%
category: 'instance creation'
classmethod: WordArray
new: size

	^(super new: size) atAllPut: 0; yourself
%
! ------------------- Instance methods for WordArray
category: 'converting'
method: WordArray
asWordArray
	^self
%
category: 'accessing'
method: WordArray
byteSize
	^self size * 4
%
category: 'accessing'
method: WordArray
bytesPerElement
	"Number of bytes in each item.  This multiplied by (self size)*8 gives the number of bits stored."
	^ 4
%
category: 'accessing'
method: WordArray
defaultElement
	"Return the default element of the receiver"
	^0
%

! Remove existing behavior from UndefinedSymbolNotification
removeallmethods UndefinedSymbolNotification
removeallclassmethods UndefinedSymbolNotification
! ------------------- Class methods for UndefinedSymbolNotification
! ------------------- Instance methods for UndefinedSymbolNotification
category: 'ANSI - signaled exception'
method: UndefinedSymbolNotification
defaultAction
	"Answer true if you don't want a compile error to be generated on encountering undefined sysmbols."

	^true.
%

! Remove existing behavior from SharedPool
removeallmethods SharedPool
removeallclassmethods SharedPool
! ------------------- Class methods for SharedPool
! ------------------- Instance methods for SharedPool

! Remove existing behavior from ZipConstants
removeallmethods ZipConstants
removeallclassmethods ZipConstants
! ------------------- Class methods for ZipConstants
category: 'pool initialization'
classmethod: ZipConstants
initialize
	"ZipConstants initialize"
	self initializeDeflateConstants.
	self initializeWriteStreamConstants.
%
category: 'pool initialization'
classmethod: ZipConstants
initializeDeflateConstants

	WindowSize _ 16r8000.
	WindowMask _ WindowSize - 1.
	MaxDistance _ WindowSize.

	MinMatch _ 3.
	MaxMatch _ 258.

	HashBits _ 15.
	HashMask _ (1 << HashBits) - 1.
	HashShift _ (HashBits + MinMatch - 1) // MinMatch.
%
category: 'pool initialization'
classmethod: ZipConstants
initializeDistanceCodes
	| dist |
	BaseDistance _ WordArray new: MaxDistCodes.
	DistanceCodes _ WordArray new: 512.
	dist _ 0.
	1 to: 16 do:[:code|
		BaseDistance at: code put: dist.
		1 to: (1 bitShift: (ExtraDistanceBits at: code)) do:[:n|
			dist _ dist + 1.
			DistanceCodes at: dist put: code-1]].
	dist = 256 ifFalse:[self error:'Whoops?!'].
	dist _ dist >> 7.
	17 to: MaxDistCodes do:[:code|
		BaseDistance at: code put: dist << 7.
		1 to: (1 bitShift: (ExtraDistanceBits at: code)-7) do:[:n|
			dist _ dist + 1.
			DistanceCodes at: 256 + dist put: code-1]].
%
category: 'pool initialization'
classmethod: ZipConstants
initializeExtraBits
	ExtraLengthBits _ 
		WordArray withAll: #(0 0 0 0 0 0 0 0 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 5 5 5 5 0).
	ExtraDistanceBits _ 
		WordArray withAll: #(0 0 0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10 11 11 12 12 13 13).
	ExtraBitLengthBits _ 
		WordArray withAll: #(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 3 7).
	BitLengthOrder _
		WordArray withAll: #(16 17 18 0 8 7 9 6 10 5 11 4 12 3 13 2 14 1 15).
%
category: 'pool initialization'
classmethod: ZipConstants
initializeFixedTrees
	"ZipWriteStream initializeFixedTrees"
	| counts nodes |
	FixedLiteralTree _ ZipEncoderTree new.
	FixedLiteralTree maxCode: 287.
	counts _ WordArray new: MaxBits+1.
	counts at: 7+1 put: 24.
	counts at: 8+1 put: 144+8.
	counts at: 9+1 put: 112.
	nodes _ Array new: 288.
	1 to: 288 do:[:i| nodes at: i put: (ZipEncoderNode value: i-1 frequency: 0 height: 0)].
	0 to: 143 do:[:i| (nodes at: i+1) setBitLengthTo: 8].
	144 to: 255 do:[:i| (nodes at: i+1) setBitLengthTo: 9].
	256 to: 279 do:[:i| (nodes at: i+1) setBitLengthTo: 7].
	280 to: 287 do:[:i| (nodes at: i+1) setBitLengthTo: 8].
	FixedLiteralTree buildCodes: nodes counts: counts maxDepth: MaxBits.
	FixedLiteralTree setValuesFrom: nodes.

	FixedDistanceTree _ ZipEncoderTree new.
	FixedDistanceTree maxCode: MaxDistCodes.
	FixedDistanceTree
		bitLengths: ((WordArray new: MaxDistCodes+1) atAllPut: 5; yourself)
		codes: ((0 to: MaxDistCodes) collect:[:i| FixedDistanceTree reverseBits: i length: 5]).
%
category: 'pool initialization'
classmethod: ZipConstants
initializeLengthCodes
	| length |
	BaseLength _ WordArray new: MaxLengthCodes.
	MatchLengthCodes _ WordArray new: MaxMatch - MinMatch + 1.
	length _ 0.
	1 to: MaxLengthCodes - 1 do:[:code|
		BaseLength at: code put: length.
		1 to: (1 bitShift: (ExtraLengthBits at: code)) do:[:n|
			length _ length + 1.
			MatchLengthCodes at: length put: NumLiterals + code]].
%
category: 'pool initialization'
classmethod: ZipConstants
initializeWriteStreamConstants

	MaxBits _ 15.
	MaxBitLengthBits _ 7.
	EndBlock _ 256.

	StoredBlock _ 0.
	FixedBlock _ 1.
	DynamicBlock _ 2.

	NumLiterals _ 256.
	MaxLengthCodes _ 29.
	MaxDistCodes _ 30.
	MaxBitLengthCodes _ 19.
	MaxLiteralCodes _ NumLiterals + MaxLengthCodes + 1. "+ End of Block"

	Repeat3To6 _ 16. "Repeat previous bit length 3-6 times (2 bits repeat count)"
	Repeat3To10 _ 17. "Repeat previous bit length 3-10 times (3 bits repeat count)"
	Repeat11To138 _ 18. "Repeat previous bit length 11-138 times (7 bits repeat count)"

	self initializeExtraBits.
	self initializeLengthCodes.
	self initializeDistanceCodes.
	self initializeFixedTrees.
%
! ------------------- Instance methods for ZipConstants

! Remove existing behavior from ZipFileConstants
removeallmethods ZipFileConstants
removeallclassmethods ZipFileConstants
! ------------------- Class methods for ZipFileConstants
category: 'pool initialization'
classmethod: ZipFileConstants
initialize
	"ZipFileConstants initialize"
	FaMsdos		:= 0.
	FaUnix 		:= 3.
	DeflatingCompressionNormal		:= 0.
	DeflatingCompressionMaximum	:= 2.
	DeflatingCompressionFast		:= 4.
	DeflatingCompressionSuperFast	:= 6.
	CompressionStored				:= 0.
	CompressionDeflated				:= 8.
	CompressionLevelNone			:= 0.
	CompressionLevelDefault			:= 6.
	IfaTextFile						:= 1.
	IfaBinaryFile					:= 0.
	DataDescriptorLength 				:= 12.

	"Unix permission bits"
	DefaultDirectoryPermissions		:= 8r040755.
	DefaultFilePermissions			:= 8r0100666.
	DirectoryAttrib 					:= 8r040000.
	FileAttrib 						:= 8r0100000.

	CentralDirectoryFileHeaderSignature _ 
		(ByteArray with: 16r50 with: 16r4B with: 16r01 with: 16r02).
	LocalFileHeaderSignature _
		(ByteArray with: 16r50 with: 16r4B with: 16r03 with: 16r04).
	EndOfCentralDirectorySignature _
		(ByteArray with: 16r50 with: 16r4B with: 16r05 with: 16r06).
%
! ------------------- Instance methods for ZipFileConstants

! Remove existing behavior from ZipEncoderNode
removeallmethods ZipEncoderNode
removeallclassmethods ZipEncoderNode
! ------------------- Class methods for ZipEncoderNode
category: 'instance creation'
classmethod: ZipEncoderNode
value: v frequency: f height: h
	^self new setValue: v frequency: f height: h
%
! ------------------- Instance methods for ZipEncoderNode
category: 'accessing'
method: ZipEncoderNode
bitLength
	^bitLength ifNil:[0]
%
category: 'accessing'
method: ZipEncoderNode
code
	^code ifNil:[0]
%
category: 'accessing'
method: ZipEncoderNode
code: aCode
	self assert:[aCode >= 0 and:[(1 bitShift: bitLength) > aCode]].
	code := aCode.
%
category: 'private'
method: ZipEncoderNode
computeHeight
	^self isLeaf
		ifTrue:[height := 0]
		ifFalse:[height := (left computeHeight max: right computeHeight) + 1].
%
category: 'encoding'
method: ZipEncoderNode
encodeBitLength: blCounts from: aTree
	| index |
	"Note: If bitLength is not nil then the tree must be broken"
	bitLength == nil ifFalse:[self error:'Huffman tree is broken'].
	parent = nil 
		ifTrue:[bitLength := 0]
		ifFalse:[bitLength := parent bitLength + 1].
	self isLeaf ifTrue:[
		index := bitLength + 1.
		blCounts at: index put: (blCounts at: index) + 1.
	] ifFalse:[
		left encodeBitLength: blCounts from: aTree.
		right encodeBitLength: blCounts from: aTree.
	].
%
category: 'accessing'
method: ZipEncoderNode
frequency
	^frequency
%
category: 'accessing'
method: ZipEncoderNode
height
	^height
%
category: 'testing'
method: ZipEncoderNode
isLeaf
	^left == nil
%
category: 'accessing'
method: ZipEncoderNode
left
	^left
%
category: 'accessing'
method: ZipEncoderNode
left: aNode
	aNode parent: self.
	left := aNode.
%
category: 'accessing'
method: ZipEncoderNode
parent
	^parent
%
category: 'accessing'
method: ZipEncoderNode
parent: aNode
	parent := aNode
%
category: 'accessing'
method: ZipEncoderNode
right
	^right
%
category: 'accessing'
method: ZipEncoderNode
right: aNode
	aNode parent: self.
	right := aNode.
%
category: 'encoding'
method: ZipEncoderNode
rotateToHeight: maxHeight
	"Rotate the tree to achieve maxHeight depth"
	| newParent |
	height < 4 ifTrue:[^self].
	self left: (left rotateToHeight: maxHeight-1).
	self right: (right rotateToHeight: maxHeight-1).
	height := (left height max: right height) + 1.
	height <= maxHeight ifTrue:[^self].
	(left height - right height) abs <= 2 ifTrue:[^self].
	left height < right height ifTrue:[
		right right height >= right left height ifTrue:[
			newParent := right.
			self right: newParent left.
			newParent left: self.
		] ifFalse:[
			newParent := right left.
			right left: newParent right.
			newParent right: right.
			self right: newParent left.
			newParent left: self.
		].
	] ifFalse:[
		left left height >= left right height ifTrue:[
			newParent := left.
			self left: newParent right.
			newParent right: self.
		] ifFalse:[
			newParent := left right.
			left right: newParent left.
			newParent left: left.
			self left: newParent right.
			newParent right: self.
		].
	].
	parent computeHeight.
	^parent
%
category: 'accessing'
method: ZipEncoderNode
setBitLengthTo: bl
	bitLength := bl
%
category: 'accessing'
method: ZipEncoderNode
setValue: v frequency: f height: h
	value := v.
	frequency := f.
	height := h.
%
category: 'accessing'
method: ZipEncoderNode
value
	^value
%

! Remove existing behavior from ZipEncoderTree
removeallmethods ZipEncoderTree
removeallclassmethods ZipEncoderTree
! ------------------- Class methods for ZipEncoderTree
category: 'instance creation'
classmethod: ZipEncoderTree
buildTreeFrom: frequencies maxDepth: depth
	^self new buildTreeFrom: frequencies maxDepth: depth
%
! ------------------- Instance methods for ZipEncoderTree
category: 'accessing'
method: ZipEncoderTree
bitLengthAt: index
	^bitLengths at: index+1
%
category: 'accessing'
method: ZipEncoderTree
bitLengths
	"Return an array of all bitLength values for valid codes"
	^bitLengths
%
category: 'private'
method: ZipEncoderTree
bitLengths: blArray codes: codeArray
	bitLengths := blArray as: WordArray.
	codes := codeArray as: WordArray.
	self assert:[(self bitLengthAt: maxCode) > 0].
%
category: 'encoding'
method: ZipEncoderTree
buildCodes: nodeList counts: blCounts maxDepth: depth
	"Build the codes for all nodes"
	| nextCode code node length |
	nextCode :=WordArray new: depth+1.
	code := 0.
	1 to: depth do:[:bits|
		code := (code + (blCounts at: bits)) << 1.
		nextCode at: bits+1 put: code].
	self assert:[(code + (blCounts at: depth+1) - 1) = (1 << depth - 1)].
	0 to: maxCode do:[:n|
		node := nodeList at: n+1.
		length := node bitLength.
		length = 0 ifFalse:[
			code := nextCode at: length+1.
			node code: (self reverseBits: code length: length).
			nextCode at: length+1 put: code+1.
		].
	].
%
category: 'encoding'
method: ZipEncoderTree
buildHierarchyFrom: aHeap
	"Build the node hierarchy based on the leafs in aHeap"
	| left right parent |
	[aHeap size > 1] whileTrue:[
		left := aHeap removeFirst.
		right := aHeap removeFirst.
		parent := ZipEncoderNode value: -1 
			frequency: (left frequency + right frequency)
			height: (left height max: right height) + 1.
		left parent: parent.
		right parent: parent.
		parent left: left.
		parent right: right.
		aHeap add: parent].
	^aHeap removeFirst
%
category: 'encoding'
method: ZipEncoderTree
buildTree: nodeList maxDepth: depth
	"Build either the literal or the distance tree"
	| heap rootNode blCounts |
	heap := SortedCollection new: nodeList size // 3.
	heap sortBlock: self nodeSortBlock.
	"Find all nodes with non-zero frequency and add to heap"
	maxCode := 0.
	nodeList do:[:dNode|
		dNode frequency = 0 ifFalse:[
			maxCode := dNode value.
			heap add: dNode]].
	"The pkzip format requires that at least one distance code exists,
	and that at least one bit should be sent even if there is only one
	possible code. So to avoid special checks later on we force at least
	two codes of non zero frequency."
	heap size = 0 ifTrue:[
		self assert:[maxCode = 0].
		heap add: nodeList first.
		heap add: nodeList second.
		maxCode := 1].
	heap size = 1 ifTrue:[
		nodeList first frequency = 0
			ifTrue:[heap add: nodeList first]
			ifFalse:[heap add: nodeList second].
		maxCode := maxCode max: 1].
	rootNode := self buildHierarchyFrom: heap.
	rootNode height > depth ifTrue:[
		rootNode := rootNode rotateToHeight: depth.
		rootNode height > depth ifTrue:[self error:'Cannot encode tree']].
	blCounts := WordArray new: depth+1.
	rootNode encodeBitLength: blCounts from: self.
	self buildCodes: nodeList counts: blCounts maxDepth: depth.
	self setValuesFrom: nodeList.
%
category: 'encoding'
method: ZipEncoderTree
buildTreeFrom: frequencies maxDepth: depth
	"Build the receiver from the given frequency values"
	| nodeList |
	nodeList := Array new: frequencies size.
	1 to: frequencies size do:[:i|
		nodeList at: i put: (ZipEncoderNode value: i-1 frequency: (frequencies at: i) height: 0)
	].
	self buildTree: nodeList maxDepth: depth.
%
category: 'accessing'
method: ZipEncoderTree
codeAt: index
	^codes at: index+1
%
category: 'accessing'
method: ZipEncoderTree
maxCode
	^maxCode
%
category: 'accessing'
method: ZipEncoderTree
maxCode: aNumber
	maxCode := aNumber.
%
category: 'encoding'
method: ZipEncoderTree
nodeSortBlock
	^[:n1 :n2|
		n1 frequency = n2 frequency
			ifTrue:[n1 height <= n2 height]
			ifFalse:[n1 frequency <= n2 frequency]].
%
category: 'private'
method: ZipEncoderTree
reverseBits: code length: length
	"Bit reverse the given code"
	| result bit bits |
	result := 0.
	bits := code.
	1 to: length do:[:i|
		bit := bits bitAnd: 1.
		result := result << 1 bitOr: bit.
		bits := bits >> 1].
	^result
%
category: 'private'
method: ZipEncoderTree
setValuesFrom: nodeList
	self bitLengths: (nodeList
			collect: [:n | n bitLength]
			from: 1
			to: maxCode + 1)
		codes: (nodeList
				collect: [:n | n code]
				from: 1
				to: maxCode + 1)
%
category: '*core-squeak'
method: Object
squeakStoreOn: aStream
	
	self storeOn: aStream
%
category: '*core-squeak'
method: Object
isCharacter

	^ false.
%
category: '*core-squeak'
method: Object
initialize
%
category: '*core-squeak'
method: Object
isOctetString
	^ false
%
category: '*core-squeak'
method: Object
ifNil: nilBlock
	"Return self, or evaluate the block if I'm == nil (q.v.)"

	^ self
%
category: '*core-squeak'
method: Object
assert: aBlock
	"Throw an assertion error if aBlock does not evaluates to true."

	aBlock value ifFalse: [self error: 'Assertion failed']
%
category: '*core-squeak'
method: Object
readDataFrom: aDataStream size: varsOnDisk
	"Fill in the fields of self based on the contents of aDataStream.  Return self.
	 Read in the instance-variables written by Object>>storeDataOn:.
	 NOTE: This method must send beginReference: before reading any objects from aDataStream that might reference it.
	 Allow aDataStream to have fewer inst vars.  See SmartRefStream."
	| cntInstVars cntIndexedVars |

	cntInstVars := self class instSize.
	self class isVariable
		ifTrue: [cntIndexedVars := varsOnDisk - cntInstVars.
				cntIndexedVars < 0 ifTrue: [
					self error: 'Class has changed too much.  Define a convertxxx method']]
		ifFalse: [cntIndexedVars := 0.
				cntInstVars := varsOnDisk]. 	"OK if fewer than now"

	aDataStream beginReference: self.
	1 to: cntInstVars do:
		[:i | self instVarAt: i put: aDataStream next].
	1 to: cntIndexedVars do:
		[:i | self basicAt: i put: aDataStream next].
	"Total number read MUST be equal to varsOnDisk!"
	^ self	"If we ever return something other than self, fix calls 
			on (super readDataFrom: aDataStream size: anInteger)"
%
category: '*core-squeak'
method: Object
storeDataOn: aDataStream
	"Store myself on a DataStream.  Answer self.  This is a low-level DataStream/ReferenceStream method. See also objectToStoreOnDataStream.  NOTE: This method must send 'aDataStream beginInstance:size:' and then (nextPut:/nextPutWeak:) its subobjects.  readDataFrom:size: reads back what we write here."
	| cntInstVars cntIndexedVars |

	cntInstVars := self class instSize.
	cntIndexedVars := self basicSize - cntInstVars.
	aDataStream
		beginInstance: self class
		size: cntInstVars + cntIndexedVars.
	1 to: cntInstVars do:
		[:i | aDataStream nextPut: (self instVarAt: i)].

	"Write fields of a variable length object."
	1 to: cntIndexedVars do:
		[:i | aDataStream nextPut: (self basicAt: i)].
%
category: '*core-squeak'
method: Object
as: aSimilarClass
	"Create an object of class aSimilarClass that has similar contents to the receiver."

	^ aSimilarClass newFrom: self
%
category: '*core-squeak'
method: Object
isString
	"Overridden to return true in String, natch"
	^ false
%
category: '*core-squeak'
method: Object
ifNotNil: ifNotNilBlock
	"Evaluate the block, unless I'm == nil (q.v.)"

	^ ifNotNilBlock value
%
category: '*core-squeak'
method: Object
objectForDataStream: refStrm
    "Return an object to store on an external data stream."

    ^ self
%
category: '*core-squeak'
method: Object
isStream

	^false
%
category: '*core-squeak'
method: Behavior
_compileMethod: source category: cat using: aSymbolList
	""
	| result errorString |
	result := self 
        compileMethod: source
        dictionaries: aSymbolList
        category: cat.
	result ~~ nil 
		ifTrue: [ | undefSymbol symbols undefinedSymbols undefinedSymbolList |
			undefSymbol := true.
			symbols := Array new.
			result do: [:errArray |
				(errArray at: 1) = 1031 
					ifTrue: [ 
						symbols add: (errArray at: 5) asSymbol.
						undefSymbol := UndefinedSymbolNotification signal]
					ifFalse: [undefSymbol := false]. 
			].
			undefSymbol
				ifTrue: [
					undefinedSymbols := aSymbolList objectNamed: #UndefinedSymbols.
					undefinedSymbols ~~ nil
						ifTrue: [
							symbols do: [:sym | | s |
								s := undefinedSymbols at: sym otherwise: nil.
								s == nil 
									ifTrue: [ 
										s := Set new.
										undefinedSymbols at: sym put: s
									].
							].
							undefinedSymbolList := aSymbolList objectNamed: #UndefinedSymbolList.
							result := self 
								compileMethod: source
								dictionaries: aSymbolList, undefinedSymbolList
								category: cat.
						]
						ifFalse: [ undefSymbol := false ].
				].
			result ~~ nil ifTrue: [ ^result ].
			undefSymbol ifTrue: [ symbols do: [:sym |  
				(undefinedSymbols at: sym) add: (self -> (Behavior parseSelector: source for: self)) ]]].
	^nil
%
category: '*core-squeak'
method: Behavior
methodsFor: categoryName stamp: aString 

	^ ClassCategoryReader new setClass: self
			category: categoryName asSymbol
			changeStamp: aString
%
category: '*core-squeak'
method: Behavior
_recompile: aSelector

	| stamp result source |
	stamp := self stampForMethod: aSelector.
	source := self sourceCodeAt: aSelector.
	[ result := self 
		compileMethod: source
		category: (self categoryOfSelector: aSelector)
		using: GsSession currentSession symbolList ] on: Warning do: [:ex | ex resume ].
	result ~~ nil
		ifTrue: [ | errorString |
			errorString := GsMethod _sourceWithErrors: result fromString:  source.
			^self error: errorString ].
	self setStamp: stamp forMethod: aSelector
%
category: '*core-squeak'
method: Behavior
commentStamp: aStamp prior: anInt

	^ ClassCommentReader new setClass: self
			category: ''
			changeStamp: aStamp
%
category: '*core-squeak'
method: Behavior
compileMethod: source category: cat

	| result errorString |
  	result := self compileMethod: source category: cat using: GsSession currentSession symbolList.
  	result ~~ nil
    		ifTrue: [
      		errorString := GsMethod _sourceWithErrors: result fromString: source.
      		^self error: 'Compile error in Class: ', self name printString, ' method: ', errorString]
%
category: '*core-squeak'
classmethod: Behavior
parseSelector: methodString for: aBehavior
	| selector mDict cDict meth undefinedSymbolList |
	mDict := GsMethodDictionary new.
	cDict := GsMethodDictionary new.
	undefinedSymbolList := GsSession currentSession symbolList objectNamed: #UndefinedSymbolList.
	undefinedSymbolList == nil ifTrue: [ undefinedSymbolList := SymbolList new ].
	meth := aBehavior
		_primitiveCompileMethod: methodString
		symbolList: GsSession currentSession symbolList, undefinedSymbolList
		category: #xxxyyz
		oldLitVars: nil 
		intoMethodDict: mDict 
		intoCategories: cDict
		intoPragmas: nil.
	meth class ~~ GsMethod 
		ifTrue: [ 
			"if error slot is nil, then the method wasn't compiled because of errors"
			(meth at: 2) == nil ifFalse: [ ^nil ].
			meth := (meth at: 1).
		].
    ^meth selector asString.
%
category: '*core-squeak'
method: Class
objectForDataStream: refStrm
	| |
	"I am about to be written on an object file.  Write a reference to a class instead."

	^ DiskProxy global: self name selector: #withClassVersion:
				args: #[self classVersion]
%
category: '*core-squeak'
method: Class
methodsFor: categoryName stamp: aString 

	self finalizeCreation.
	^ super methodsFor: categoryName stamp: aString
%
category: '*core-squeak'
method: Class
classVersion
	"Default.  Any class may return a later version to inform readers that use ReferenceStream.  8/17/96 tk"
	"This method allows you to distinguish between class versions when the shape of the class 
	hasn't changed (when there's no change in the instVar names).
	In the conversion methods you usually can tell by the inst var names 
	what old version you have. In a few cases, though, the same inst var 
	names were kept but their interpretation changed (like in the layoutFrame).
	By changing the class version when you keep the same instVars you can 
	warn older and newer images that they have to convert."
	^ 0
%
category: '*core-squeak'
method: Class
commentStamp: aStamp prior: anInt

	self finalizeCreation.
	^super commentStamp: aStamp prior: anInt
%
category: '*core-squeak'
method: Class
storeDataOn: aDataStream
	"I don't get stored.  Use a DiskProxy"

	self error: 'use a DiskProxy to store a Class'
%
category: '*core-squeak-subclass creation'
method: Class
indexableSubclass: aString
instVarNames: anArrayOfStrings
classVars: anArrayOfClassVars
classInstVars: anArrayOfClassInstVars
poolDictionaries: anArrayOfPoolDicts
inDictionary: aDictionaryName
category: aCategoryName

	^Class _defineClassNamed: aString with: [ | symbolDict |
		symbolDict := aDictionaryName isEmpty
			ifTrue: [GsPackagePolicy current homeSymbolDict]
			ifFalse: [System myUserProfile symbolList objectNamed: aDictionaryName asSymbol].
		self indexableSubclass: aString
			instVarNames: anArrayOfStrings
			classVars: anArrayOfClassVars
			classInstVars: anArrayOfClassInstVars
			poolDictionaries: anArrayOfPoolDicts
			category: aCategoryName
			inDictionary: symbolDict
			instancesInvariant: false
			isModifiable: false ]
%
category: '*core-squeak-subclass creation'
method: Class
subclass: aString
instVarNames: anArrayOfStrings
classVars: anArrayOfClassVars
classInstVars: anArrayOfClassInstVars
poolDictionaries: anArrayOfPoolDicts
inDictionary: aDictionaryName
category: aCategoryName

	^Class _defineClassNamed: aString with: [ | symbolDict |
		symbolDict := aDictionaryName isEmpty
			ifTrue: [GsPackagePolicy current homeSymbolDict]
			ifFalse: [System myUserProfile symbolList objectNamed: aDictionaryName asSymbol].
		self subclass: aString
			instVarNames: anArrayOfStrings
			classVars: anArrayOfClassVars
			classInstVars: anArrayOfClassInstVars
			poolDictionaries: anArrayOfPoolDicts
			category: aCategoryName
			inDictionary: symbolDict
			constraints: #[]
			instancesInvariant: false
			isModifiable: false].
%
category: '*core-squeak-subclass creation'
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
category: '*core-squeak-subclass creation'
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

"clear the ClassOrganizer cache"
ClassOrganizer clearCachedOrganizer.

^ result
%
category: '*core-squeak-subclass creation'
method: Class
byteSubclass: aString
classVars: anArrayOfClassVars
classInstVars: anArrayOfClassInstVars
poolDictionaries: anArrayOfPoolDicts
category: aCategoryName

	^self byteSubclass: aString
		classVars: anArrayOfClassVars
		classInstVars: anArrayOfClassInstVars
		poolDictionaries: anArrayOfPoolDicts
		inDictionary: ''
		category: aCategoryName
%
category: '*core-squeak-subclass creation'
method: Class
_resolveUndeclaredSymbolsFor: aSymbol
	| undefinedSymbols |
	undefinedSymbols := GsSession currentSession symbolList objectNamed: #UndefinedSymbols.
	undefinedSymbols ~~ nil
		ifTrue: [
			(undefinedSymbols at: aSymbol otherwise: Set new) do: [:assoc | 
				(assoc key includesSelector: assoc value) 
					ifTrue: [ assoc key _recompile: assoc value ]].
			(undefinedSymbols at: aSymbol otherwise: Set new) isEmpty 
				ifTrue: [ undefinedSymbols removeKey: aSymbol ifAbsent: []]].
%
category: '*core-squeak-subclass creation'
method: Class
indexableSubclass: aString
instVarNames: anArrayOfStrings
classVars: anArrayOfClassVars
classInstVars: anArrayOfClassInstVars
poolDictionaries: anArrayOfPoolDicts
category: aCategoryName

	^self indexableSubclass: aString
		instVarNames: anArrayOfStrings
		classVars: anArrayOfClassVars
		classInstVars: anArrayOfClassInstVars
		poolDictionaries: anArrayOfPoolDicts
		inDictionary: ''
		category: aCategoryName
%
category: '*core-squeak-subclass creation'
method: Class
variableByteSubclass: subclassName
	instanceVariableNames: instanceVariableNames
	classVariableNames: classVariableNames
	poolDictionaries: poolDictionaryNames
	category: category

  ^self byteSubclass: subclassName
	classVars: (classVariableNames findTokens: ' ')  asArray
	classInstVars: #()
	poolDictionaries: (self poolDictionariesForNames: (poolDictionaryNames findTokens: ' ') asArray)
	inDictionary: ''
	category: category
%
category: '*core-squeak-subclass creation'
method: Class
subclass: aString
instVarNames: anArrayOfStrings
classVars: anArrayOfClassVars
classInstVars: anArrayOfClassInstVars
poolDictionaries: anArrayOfPoolDicts
category: aCategoryName

	^self subclass: aString
		instVarNames: anArrayOfStrings
		classVars: anArrayOfClassVars
		classInstVars: anArrayOfClassInstVars
		poolDictionaries: anArrayOfPoolDicts
		inDictionary: ''
		category: aCategoryName
%
category: '*core-squeak-subclass creation'
method: Class
transientSubclass: aString
instVarNames: anArrayOfStrings
classVars: anArrayOfClassVars
classInstVars: anArrayOfClassInstVars
poolDictionaries: anArrayOfPoolDicts
category: aCategoryName

	^self transientSubclass: aString
		instVarNames: anArrayOfStrings
		classVars: anArrayOfClassVars
		classInstVars: anArrayOfClassInstVars
		poolDictionaries: anArrayOfPoolDicts
		inDictionary: ''
		category: aCategoryName
%
category: '*core-squeak-subclass creation'
method: Class
transientSubclass: subclassName
	instanceVariableNames: instanceVariableNames
	classVariableNames: classVariableNames
	poolDictionaries: poolDictionaryNames
	category: category

	^self transientSubclass: subclassName
		instVarNames: (instanceVariableNames findTokens: ' ') asArray
		classVars: (classVariableNames findTokens: ' ')  asArray
		classInstVars: #()
		poolDictionaries: (self poolDictionariesForNames: (poolDictionaryNames findTokens: ' ') asArray)
		inDictionary: ''
		category: category
%
category: '*core-squeak-subclass creation'
method: Class
variableSubclass: subclassName
	instanceVariableNames: instanceVariableNames
	classVariableNames: classVariableNames
	poolDictionaries: poolDictionaryNames
	category: category

	^self indexableSubclass: subclassName
		instVarNames: (instanceVariableNames findTokens: ' ') asArray
		classVars: (classVariableNames findTokens: ' ')  asArray
		classInstVars: #()
		poolDictionaries: (self poolDictionariesForNames: (poolDictionaryNames findTokens: ' ') asArray)
		inDictionary: ''
		category: category
%
category: '*core-squeak-subclass creation'
method: Class
transientSubclass: aString
instVarNames: anArrayOfStrings
classVars: anArrayOfClassVars
classInstVars: anArrayOfClassInstVars
poolDictionaries: anArrayOfPoolDicts
category: aCategoryName
inDictionary: aDictionary
constraints: aConstraint
instancesInvariant: invarBoolean
isModifiable: modifyBoolean

| cl |
cl := self subclass: aString
	instVarNames: anArrayOfStrings
	classVars: anArrayOfClassVars
	classInstVars: anArrayOfClassInstVars
	poolDictionaries: anArrayOfPoolDicts
	inDictionary: aDictionary
	constraints: aConstraint
	instancesInvariant: invarBoolean
	isModifiable: true.
cl category: aCategoryName asString.
cl makeInstancesDbTransient.
modifyBoolean ifFalse:[ cl immediateInvariant ] .
^cl
%
category: '*core-squeak-subclass creation'
method: Class
byteSubclass: aString
classVars: anArrayOfClassVars
classInstVars: anArrayOfClassInstVars
poolDictionaries: anArrayOfPoolDicts
inDictionary: aDictionaryName
category: aCategoryName

	^Class _defineClassNamed: aString with: [ | symbolDict |
		symbolDict := aDictionaryName isEmpty
			ifTrue: [GsPackagePolicy current homeSymbolDict]
			ifFalse: [System myUserProfile symbolList objectNamed: aDictionaryName asSymbol].
		self byteSubclass: aString
			classVars: anArrayOfClassVars
			classInstVars: anArrayOfClassInstVars
			poolDictionaries: anArrayOfPoolDicts
			category: aCategoryName
			inDictionary: symbolDict
			instancesInvariant: false
			isModifiable: false ]
%
category: '*core-squeak-subclass creation'
method: Class
subclass: subclassName
	instanceVariableNames: instanceVariableNames
	classVariableNames: classVariableNames
	poolDictionaries: poolDictionaryNames
	category: category

	^self subclass: subclassName
		instVarNames: (instanceVariableNames findTokens: ' ') asArray
		classVars: (classVariableNames findTokens: ' ')  asArray
		classInstVars: #()
		poolDictionaries: (self poolDictionariesForNames: (poolDictionaryNames findTokens: ' ') asArray)
		inDictionary: ''
		category: category
%
category: '*core-squeak-subclass creation'
method: Class
transientSubclass: aString
instVarNames: anArrayOfStrings
classVars: anArrayOfClassVars
classInstVars: anArrayOfClassInstVars
poolDictionaries: anArrayOfPoolDicts
inDictionary: aDictionaryName
category: aCategoryName

	^Class _defineClassNamed: aString with: [ | symbolDict |
		symbolDict := aDictionaryName isEmpty
			ifTrue: [GsPackagePolicy current homeSymbolDict]
			ifFalse: [System myUserProfile symbolList objectNamed: aDictionaryName asSymbol].
		self transientSubclass: aString
			instVarNames: anArrayOfStrings
			classVars: anArrayOfClassVars
			classInstVars: anArrayOfClassInstVars
			poolDictionaries: anArrayOfPoolDicts
			category: aCategoryName
			inDictionary: symbolDict
			constraints: #[]
			instancesInvariant: false
			isModifiable: false ]
%
category: '*core-squeak-subclass creation'
method: Class
_resolveUndeclaredSymbolsForClass: cl
	
	self _resolveUndeclaredSymbolsFor: cl name asSymbol
%
category: '*core-squeak-subclass creation'
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
category: '*core-squeak'
method: Metaclass
instanceVariableNames: instanceVariableNames

	^self thisClass isInvariant
		ifTrue: [
			self thisClass _beVariantWhile: [
				self addInstVarNames: (instanceVariableNames findTokens: ' ') asArray]]
		ifFalse: [ self addInstVarNames: (instanceVariableNames findTokens: ' ') asArray ].
%
category: '*core-squeak'
classmethod: Collection
with: firstObject with: secondObject with: thirdObject with: fourthObject with: fifthObject
	"Answer an instance of me, containing the five arguments as the elements."

	^ self new
		add: firstObject;
		add: secondObject;
		add: thirdObject;
		add: fourthObject;
		add: fifthObject;
		yourself
%
category: '*core-squeak'
method: SequenceableCollection
squeakStoreOn: aStream

	aStream nextPutAll: '(('.
	aStream nextPutAll: self class name.
	aStream nextPutAll: ' new: '.
	aStream store: self size.
	aStream nextPut: $).
	(self storeElementsFrom: 1 to: self size on: aStream)
		ifFalse: [aStream nextPutAll: '; yourself'].
	aStream nextPut: $)
%
category: '*core-squeak'
method: SequenceableCollection
allButFirst: n
	"Answer a copy of the receiver containing all but the first n
	elements. Raise an error if there are not enough elements."

	^ self copyFrom: n + 1 to: self size
%
category: '*core-squeak'
method: SequenceableCollection
allButFirst
	"Answer a copy of the receiver containing all but the first
	element. Raise an error if there are not enough elements."

	^ self allButFirst: 1
%
category: '*core-squeak'
method: SequenceableCollection
beginsWith: aSequenceableCollection

	(aSequenceableCollection isEmpty or: [self size < aSequenceableCollection size]) ifTrue: [^false].
	aSequenceableCollection withIndexDo: [:each :index | (self at: index) ~= each ifTrue: [^false]].
	^true
%
category: '*core-squeak'
method: SequenceableCollection
pairsDo: aBlock 
	"Evaluate aBlock with my elements taken two at a time.  If there's an odd number of items, ignore the last one.  Allows use of a flattened array for things that naturally group into pairs.  See also pairsCollect:"

	1 to: self size // 2 do:
		[:index | aBlock value: (self at: 2 * index - 1) value: (self at: 2 * index)]
"
#(1 'fred' 2 'charlie' 3 'elmer') pairsDo:
	[:a :b | Transcript cr; show: b, ' is number ', a printString]
"
%
category: '*core-squeak'
method: SequenceableCollection
fourth
	"Answer the fourth element of the receiver.
	Raise an error if there are not enough elements."

	^ self checkedAt: 4
%
category: '*core-squeak'
method: SequenceableCollection
withIndexDo: elementAndIndexBlock 
	"Just like with:do: except that the iteration index supplies the second argument to the block."
	1 to: self size do:
		[:index |
		elementAndIndexBlock
			value: (self at: index)
			value: index]
%
category: '*core-squeak'
method: SequenceableCollection
storeElementsFrom: firstIndex to: lastIndex on: aStream

	| noneYet defaultElement arrayElement |
	noneYet _ true.
	defaultElement _ self squeakDefaultElement.
	firstIndex to: lastIndex do: 
		[:index | 
		arrayElement _ self at: index.
		arrayElement = defaultElement
			ifFalse: 
				[noneYet
					ifTrue: [noneYet _ false]
					ifFalse: [aStream nextPut: $;].
				aStream nextPutAll: ' at: '.
				aStream store: index.
				aStream nextPutAll: ' put: '.
				aStream store: arrayElement]].
	^noneYet
%
category: '*core-squeak'
method: SequenceableCollection
at: index ifAbsent: exceptionBlock 
	"Answer the element at my position index. If I do not contain an element 
	at index, answer the result of evaluating the argument, exceptionBlock."

	(index between: 1 and: self size) ifTrue: [^ self at: index].
	^ exceptionBlock value
%
category: '*core-squeak'
method: SequenceableCollection
squeakDefaultElement

	^nil
%
category: '*core-squeak'
method: SequenceableCollection
checkedAt: index
	index > self size ifTrue: [self error: 'not enough elements'].
	^ self at: index
%
category: '*core-squeak'
method: SequenceableCollection
first
	"Answer the first element of the receiver.
	Raise an error if the collection is empty."

	self size = 0 ifTrue: [self errorEmptyCollection].
	^ self at: 1
%
category: '*core-squeak'
method: SequenceableCollection
second
	"Answer the second element of the receiver.
	Raise an error if there are not enough elements."

	^ self checkedAt: 2
%
category: '*core-squeak'
method: SequenceableCollection
third
	"Answer the third element of the receiver.
	Raise an error if there are not enough elements."

	^ self checkedAt: 3
%
category: '*core-squeak'
method: SequenceableCollection
collect: aBlock from: firstIndex to: lastIndex
	"Refer to the comment in Collection|collect:."

	| size result j |
	size := lastIndex - firstIndex + 1.
	result := self species new: size.
	j := firstIndex.
	1 to: size do: [:i | result at: i put: (aBlock value: (self at: j)). j := j + 1].
	^ result
%
category: '*core-squeak'
method: SequenceableCollection
squeakReplaceFrom: startIndex to: stopIndex with: aSeqCollection startingAt: repIndex

"Replaces the elements of the receiver between the indexes startIndex and 
 stopIndex inclusive with the elements of aSeqCollection starting at startIndex. 
 Returns the receiver....Vanilla algorithm is _required_"

| j |
j := repIndex.    
startIndex to: stopIndex do: [:i|
  self at: i put: (aSeqCollection at: j).
  j := j + 1.
  ].

^ self.
%
category: '*core-squeak'
classmethod: SequenceableCollection
streamContents: blockWithArg
	| stream |
	stream := WriteStream on: (self new).
	blockWithArg value: stream.
	^stream contents
%
category: '*core-squeak'
classmethod: SequenceableCollection
new: size withAll: value 
	"Answer an instance of me, with number of elements equal to size, each 
	of which refers to the argument, value."

	^(self new: size) atAllPut: value; yourself
%
category: '*core-squeak'
method: CharacterCollection
findDelimiters: delimiters startingAt: start 
	"Answer the index of the character within the receiver, starting at start, that matches one of the delimiters. If the receiver does not contain any of the delimiters, answer size + 1."

	start to: self size do: [:i |
		delimiters do: [:delim | delim = (self at: i) ifTrue: [^ i]]].
	^ self size + 1
%
category: '*core-squeak'
method: CharacterCollection
asOctetString

	| res |
	res := self _asString .
	res == nil ifTrue:[ ^ self ].
	^ res
%
category: '*core-squeak'
method: CharacterCollection
isOctetString
	"Answer whether the receiver can be represented as a byte string."

	^self _asString ~~ nil
%
category: '*core-squeak'
method: CharacterCollection
withGemstoneLineEndings
	"assume the string is textual, and that CR, LF, and CRLF are all 
	valid line endings.  Replace each occurence with a single LF"
	| cr lf input c crlf inPos outPos outString lineEndPos newOutPos |
	cr := Character cr.
	lf := Character lf.
	crlf := ByteArray new.
	crlf add: cr asciiValue; add: lf asciiValue.

	inPos := 1.
	outPos := 1.
	outString := self class _newString: self size.

	[ lineEndPos := self indexOfAnyOf: crlf startingAt: inPos ifAbsent: [0].
		lineEndPos ~= 0 ] whileTrue: [
			newOutPos := outPos + (lineEndPos - inPos + 1).
			outString replaceFrom: outPos to: newOutPos - 2 with: self startingAt: inPos.
			outString at: newOutPos-1 put: lf.
			outPos := newOutPos.

			((self at: lineEndPos) = cr and: [ lineEndPos < self size and: [ (self at: lineEndPos+1) = lf ] ]) ifTrue: [
				"CRLF ending"
				inPos := lineEndPos + 2 ]
			ifFalse: [ 
				"CR or LF ending"
				inPos := lineEndPos + 1 ]. ].

	"no more line endings.  copy the rest"
	newOutPos := outPos + (self size - inPos + 1).
	outString replaceFrom: outPos to: newOutPos-1 with: self startingAt: inPos.

	^outString copyFrom: 1 to: newOutPos-1
	
%
category: '*core-squeak'
method: CharacterCollection
findTokens: delimiters
	"Answer the collection of tokens that result from parsing self.  Return strings between the delimiters.  Any character in the Collection delimiters marks a border.  Several delimiters in a row are considered as just one separation.  Also, allow delimiters to be a single character."

	| tokens keyStart keyStop separators |

	tokens _ OrderedCollection new.
	separators _ delimiters isCharacter 
		ifTrue: [Array with: delimiters]
		ifFalse: [delimiters].
	keyStop _ 1.
	[keyStop <= self size] whileTrue:
		[keyStart _ self skipDelimiters: separators startingAt: keyStop.
		keyStop _ self findDelimiters: separators startingAt: keyStart.
		keyStart < keyStop
			ifTrue: [tokens add: (self copyFrom: keyStart to: (keyStop - 1))]].
	^tokens
%
category: '*core-squeak'
method: CharacterCollection
squeakMatch: aString
	"# and * are the special characters"
	| special pattern keyStart keyStop char |
	special := #[ $#, $*].
	
	pattern := OrderedCollection new.	
	keyStart := keyStop := 1.
	[keyStop <= self size] whileTrue:
		[keyStop _ self findDelimiters: special startingAt: keyStart.
		keyStart <= keyStop
			ifTrue: [
				keyStart = keyStop
					ifTrue: [ keyStart := keyStart + 1 ]
					ifFalse: [ 
						pattern add: (self copyFrom: keyStart to: (keyStop - 1)).
						keyStart := keyStop + 1. ].
				(keyStop <= self size) 
					ifTrue: [
						char := self at: keyStop.
						char == $# ifTrue: [ char := $? ].
						pattern add: char ]]].
	^aString _matchPatternNoCase: pattern asArray
%
category: '*core-squeak'
method: CharacterCollection
asHex

	^self asHexString
%
category: '*core-squeak'
method: CharacterCollection
skipDelimiters: delimiters startingAt: start 
	"Answer the index of the character within the receiver, starting at start, that does NOT match one of the delimiters. If the receiver does not contain any of the delimiters, answer size + 1.  Assumes the delimiters to be a non-empty string."

	start to: self size do: [:i |
		delimiters detect: [:delim | delim = (self at: i)]
				ifNone: [^ i]].
	^ self size + 1
%
category: '*core-squeak'
method: CharacterCollection
isString
	^ true
%
category: '*core-squeak'
method: CharacterCollection
_asString
	"Compatability method to avoid version specific branch to support QuadByteStrings for 2.3"
	
	^nil "character size can be greater than a single byte"
%
category: '*core-squeak'
method: CharacterCollection
match: aString

	^self squeakMatch: aString
%
category: '*core-squeak'
method: CharacterCollection
squeakStoreOn: aStream 
	"Print inside string quotes, doubling inbedded quotes."
	| x |
	aStream nextPut: $'.
	1 to: self size do:
		[:i |
		aStream nextPut: (x := self at: i).
		x = $' ifTrue: [aStream nextPut: x]].
	aStream nextPut: $'
%
category: '*core-squeak'
method: CharacterCollection
indexOfAnyOf: aByteArray  startingAt: start ifAbsent: aBlock
	"returns the index of the first character in the given set, starting from start"

	| ans |
	ans := self class findFirstInString: self  inSet: aByteArray byteArrayMap startingAt: start.

	ans = 0 
		ifTrue: [ ^aBlock value ]
		ifFalse: [ ^ans ]
%
category: '*core-squeak'
method: CharacterCollection
beginsWith: prefix
	"Answer whether the receiver begins with the given prefix string.
	The comparison is case-sensitive."
	"Not the same implementation as in Squeak"

	| stream |

	self size < prefix size ifTrue: [^ false].
	stream := self readStream.
	^prefix allSatisfy: [:each | each = stream next]
%
category: '*core-squeak'
classmethod: CharacterCollection
findFirstInString: aString inSet: inclusionMap startingAt: start
	"Trivial, non-primitive version"
	| i stringSize ascii more |
	inclusionMap size ~= 256 ifTrue: [ ^0 ].

	i := start.
	stringSize := aString size.
	[ i <= stringSize and: [
		ascii := (aString at: i) asciiValue.
		ascii < 256
			ifTrue: [ (inclusionMap at: ascii+1) = 0 ]
			ifFalse: [ true ]] ] whileTrue: [ 
		i := i + 1 ].

	i > stringSize ifTrue: [ ^0 ].
	^i
%
category: '*core-squeak'
method: String
asOctetString
  ^ self
%
category: '*core-squeak'
method: String
isOctetString
  ^ true  
%
category: '*core-squeak'
method: Symbol
squeakStoreOn: aStream 

	aStream nextPut: $#.
	super squeakStoreOn: aStream
%
category: '*core-squeak'
classmethod: Symbol
hasInterned: aString ifTrue: symBlock 
	"Answer with false if aString hasn't been interned (into a Symbol), 
	otherwise supply the symbol to symBlock and answer true."

	| sym |
	sym := self _existingWithAll: aString.
	^sym == nil
		ifTrue: [false]
		ifFalse: [symBlock value: sym.  true]
%
category: '*core-squeak'
method: OrderedCollection
readDataFrom: aDataStream size: varsOnDisk
	"OrderedCollection in Squeak is a different shape. First iv is an array, second iv is firstIndex and second iv is lastIndex."
	| cntInstVars cntIndexedVars ar firstIndex lastIndex |

	cntInstVars := self class instSize.
	self class isVariable
		ifTrue: [cntIndexedVars := varsOnDisk - cntInstVars.
				cntIndexedVars < 0 ifTrue: [
					self error: 'Class has changed too much.  Define a convertxxx method']]
		ifFalse: [cntIndexedVars := 0.
				cntInstVars := varsOnDisk]. 	"OK if fewer than now"

	aDataStream beginReference: self.
	ar := aDataStream next.
	firstIndex := aDataStream next.
	lastIndex := aDataStream next.
	firstIndex <= lastIndex
		ifTrue: [ 
			"reset size to correct value"
			self size: (lastIndex - firstIndex + 1).
			ar copyFrom: firstIndex count: (lastIndex - firstIndex + 1) into: self startingAt: 1.
		 ]
		ifFalse: [
			"empty OrderedCollection"
			self size: 0.
		].
	^ self
%
category: '*core-squeak'
method: OrderedCollection
storeDataOn: aDataStream
	"OrderedCollection in Squeak is a different shape. First iv is an array, second iv is firstIndex and second iv is lastIndex."

	aDataStream
		beginInstance: self class
		size: 3.
	"array"
	aDataStream nextPut: self asArray.
	"firstIndex"
	aDataStream nextPut: 1.
	"lastIndex"
	aDataStream nextPut: self size.
%
category: '*core-squeak'
method: SortedCollection
readDataFrom: aDataStream size: varsOnDisk

self halt: 'not yet implemented'
%
category: '*core-squeak'
method: ByteArray
asString
        "Convert to a String with Characters for each byte"

        | str index |
        str := String new: self size.
        1 to: self size do: [:i |str at: i put: (Character value: (self at: i))].
        ^str
%
category: '*core-squeak'
method: ByteArray
lastIndexOfPKSignature: aSignature
	"Answer the last index in me where aSignature (4 bytes long) occurs, or 0 if not found"
	| a b c d |
	a := aSignature first.
	b := aSignature second.
	c := aSignature third.
	d := aSignature fourth.
	(self size - 3) to: 1 by: -1 do: [ :i |
		(((self at: i) = a)
			and: [ ((self at: i + 1) = b)
				and: [ ((self at: i + 2) = c)
					and: [ ((self at: i + 3) = d) ]]])
						ifTrue: [ ^i ]
	].
	^0
%
category: '*core-squeak'
method: ByteArray
byteArrayMap
	"return a ByteArray mapping each ascii value to a 1 if that ascii value is in the set, and a 0 if it isn't.  Intended for use by primitives only"
	| map |
	map := ByteArray new: 256 withAll: 0.
	self do: [:ascii | map at: ascii + 1 put: 1].
	^map
%
category: '*core-squeak'
method: ByteArray
squeakDefaultElement

	^0
%
category: '*core-squeak'
method: AbstractDictionary
at: key ifPresent: aBlock
	"Lookup the given key in the receiver. If it is present, answer the value of evaluating the given block with the value associated with the key. Otherwise, answer nil."

	| v |
	v := self at: key ifAbsent: [^ nil].
	^ aBlock value: v
%
category: '*core-squeak'
method: Number
squeakStoreOn: aStream

	self printOn: aStream
%
category: '*core-squeak'
method: Integer
digitLength
	"Returns the number of base 256 digits in the receiver, without counting
	 any leading zeros."

	| abs n |
	abs := self abs.
	n := 1.
	[	abs < (1 bitShift: n*8) ifTrue: [ ^n ].
		n := n +1.
		true ] whileTrue
%
category: '*core-squeak'
method: Integer
>> shiftAmount  "right shift"
	shiftAmount < 0 ifTrue: [self error: 'negative arg'].
	^ self bitShift: 0 - shiftAmount
%
category: '*core-squeak'
method: Integer
normalize
	"Check for leading zeroes and return shortened copy if so"
	| sLen val len oldLen new |
	self isSpecial ifTrue: [ "SmallInteger" ^self ]. 
	"First establish len = significant length"
	len _ oldLen _ self digitLength.
	[len = 0 ifTrue: [^0].
	(self digitAt: len) = 0]
		whileTrue: [len _ len - 1].

	"Now check if in SmallInteger range"
	sLen _ SmallInteger maxVal digitLength.
	(len <= sLen
		and: [(self digitAt: sLen) <= (SmallInteger maxVal digitAt: sLen)])
		ifTrue: ["If so, return its SmallInt value"
				val _ 0.
				len to: 1 by: -1 do:
					[:i | val _ (val *256) + (self digitAt: i)].
				^ val].

	"Return self, or a shortened copy"
	len < oldLen ifFalse: [^ self].
	"in-line growTo:"
	new := self class _new: len neg: self negative.
	1 to: len do: [:i | new digitAt: i put: (self digitAt: i) ].
	^new
%
category: '*core-squeak'
method: Integer
digitAt: n put: value 
	" Insert the value into the nth base 256 digit of the receiver. 
	  GemStone natively uses base 32678, do can't do a direct store."
	
	| int |
	value == 0 ifTrue: [ ^self ].
	int := value bitShift: (n-1)*8.
	1 to: int _digitLength do: [:i | | dig ndig |
		(ndig := int _digitAt: i) > 0
			ifTrue: [ 
				dig := self _digitAt: i.
				self _digitAt: i put: (dig + ndig) ]].
%
category: '*core-squeak'
method: Integer
bitInvert32
	"Answer the 32-bit complement of the receiver."

	^ self bitXor: 16rFFFFFFFF
%
category: '*core-squeak'
method: Integer
digitAt: n
	"Answer the nth base 256 digit in the receiver."

	self < 0
		ifTrue: [ ^((0-self) bitShift: (1-n)*8) bitAnd: 16rFF ]
		ifFalse: [ ^(self bitShift: (1-n)*8) bitAnd: 16rFF ]
%
category: '*core-squeak'
method: Integer
hex

	^self asHexString
%
category: '*core-squeak'
method: Integer
<< shiftAmount  "left shift"
	shiftAmount < 0 ifTrue: [self error: 'negative arg'].
	^ self bitShift: shiftAmount
%
category: '*core-squeak'
classmethod: Integer
readFrom: aStream base: base 
	"Answer an instance of one of the concrete subclasses if Integer. 
	Initial minus sign accepted, and bases > 10 use letters A-Z.
	Imbedded radix specifiers not allowed;  use Number 
	class readFrom: for that. Answer zero if there are no digits."

| factor result dv  |
factor := 1.
result := 0.
(aStream peek isEquivalent: $-)
ifTrue: [
  aStream next.
  factor := -1.]
ifFalse: [
  (aStream peek isEquivalent: $+) ifTrue: [
    aStream next]].
[ aStream atEnd not and: [ (dv := aStream peek digitValueInRadix: base) ~~ nil ] ] whileTrue: [
  aStream next.
  result := result * base + dv.
].

^ result * factor
%
category: '*core-squeak'
classmethod: Integer
readFrom: aStream 
	"Answer a new Integer as described on the stream, aStream.
	Embedded radix specifiers not allowed - use Number readFrom: for that."
	^self readFrom: aStream base: 10
%
category: '*core-squeak'
classmethod: SmallInteger
maxVal

^self maximumValue
%
category: '*core-squeak'
classmethod: SmallInteger
minVal

^self minimumValue
%
category: '*core-squeak'
method: Character
charCode
	"assuming Unicode"
	
	^(self asciiValue bitAnd: 16r3FFFFF)
%
category: '*core-squeak'
method: Character
isCharacter

	^ true.
%
category: '*core-squeak'
method: Character
hex

	^self asciiValue hex
%
category: '*core-squeak'
classmethod: Character
value: anInteger

^self withValue: anInteger
%
category: '*core-squeak'
classmethod: Time
dateAndTimeFromSeconds: secondCount
	| dateAndTime |
	dateAndTime := DateAndTime fromSeconds: secondCount.
	^ Array
		with: dateAndTime asDate
		with: dateAndTime asTime
%
category: '*core-squeak'
classmethod: Time
totalSeconds

"Returns an Integer that represents the receiver in units of seconds since
 midnight January 1, 1901, UTC (not local, because local time is too expensive)."

^(DateAndTime secondsUTC: DateAndTime secondsSince2001 offset: Duration zero) asSeconds asInteger  + 3155760000
%
category: '*core-squeak'
classmethod: Time
readFrom: aStream

	"Read a Time from the stream in the form:
		<hour>:<minute>:<second> <am/pm>
	<minute>, <second> or <am/pm> may be omitted.  e.g. 1:59:30 pm; 8AM; 15:30"

	| hour minute second ampm totalSeconds |
	hour := Integer readFrom: aStream.
	minute := 0.
	second := 0.
	(aStream peekFor: $:) ifTrue:
	[ minute := Integer readFrom: aStream.
		(aStream peekFor: $:) ifTrue: [ second := Integer readFrom: aStream ]].
	aStream skipSeparators.
	(aStream atEnd not and: [aStream peek isLetter]) ifTrue: 
		[ampm := aStream next asLowercase.
	(ampm = $p and: [hour < 12]) ifTrue: [hour := hour + 12].
		(ampm = $a and: [hour = 12]) ifTrue: [hour := 0].
	(aStream peekFor: $m) ifFalse: [aStream peekFor: $M ]].

	totalSeconds := (hour * 3600) + (minute * 60) + second.
	^ self fromSeconds: totalSeconds .

	"Time readFrom: (ReadStream on: '2:23:09 pm')"
%
category: '*core-squeak'
classmethod: DateAndTime
epoch
    "Answer a DateAndTime representing the Squeak epoch: 1 January 1901"

    ^DateAndTime year: 1901 day: 1 hour: 0 minute: 0 second: 0 offset: Duration zero
%
category: '*core-squeak'
classmethod: DateAndTime
fromSeconds: seconds
  "Answer a DateAndTime since the Squeak epoch: 1 January 1901"

  ^self epoch + (Duration seconds: seconds)
%
category: '*core-squeak'
method: Stream
isStream

	^true
%
category: '*core-squeak'
method: Stream
closed
	^false
%
category: '*core-squeak'
method: PositionableStream
setToEnd
	"Set the position of the receiver to the end of the sequence of objects."

	position := itsCollection size + 1
%
category: '*core-squeak'
method: PositionableStream
binary
	"For compatibility"
	^self.
%
category: '*core-squeak'
method: PositionableStream
nextLittleEndianNumber: n 
	"Answer the next n bytes as a positive Integer or LargePositiveInteger, where the bytes are ordered from least significant to most significant."

	| bytes s |
	bytes := self next: n.
	s := 0.
	n to: 1 by: -1 do: [:i | s := (s bitShift: 8) bitOr: (bytes at: i)].
	^ s
%
category: '*core-squeak'
method: PositionableStream
next: n into: aCollection startingAt: startIndex
	"Read n objects into the given collection. 
	Return aCollection or a partial copy if less than
	n elements have been read."
	0 to: n-1 do:[:i|
		self atEnd ifTrue: [ ^aCollection copyFrom: 1 to: startIndex+i-1 ].
		aCollection at: startIndex+i put: self next].
	^aCollection
%
category: '*core-squeak'
method: PositionableStream
nextInt32
	"Read a 32-bit signed integer from the next 4 bytes"
	| s |
	s := 0.
	1 to: 4 do: [:i | s := (s bitShift: 8) + self next].
	(s bitAnd: 16r80000000) = 0
		ifTrue: [^ s]
		ifFalse: [^ -1 - s bitInvert32]
%
category: '*core-squeak'
method: PositionableStream
nextInto: aCollection
	"Read the next elements of the receiver into aCollection.
	Return aCollection or a partial copy if less than aCollection
	size elements have been read."
	^self next: aCollection size into: aCollection startingAt: 1.
%
category: '*core-squeak'
method: PositionableStream
nextString
	"Read a string from the receiver. The first byte is the length of the string, unless it is greater than 192, in which case the first four bytes encode the length.  I expect to be in ascii mode when called (caller puts back to binary)."

	| length aByteArray |

	"read the length in binary mode"
	self binary.
	length := self next.		"first byte."
	length >= 192 ifTrue: [length := length - 192.
		1 to: 3 do: [:ii | length := length * 256 + self next]].
	aByteArray := ByteArray new: length.

	self nextInto: aByteArray.
	^aByteArray asString.
%
category: '*core-squeak'
method: PositionableStream
nextNumber: n 
	"Answer the next n bytes as a positive Integer or LargePositiveInteger."
	| s |
	s := 0.
	1 to: n do: 
		[:i | s := (s bitShift: 8) bitOr: self next asInteger].
	^ s normalize
%
category: '*core-squeak'
method: PositionableStream
nextChunk
	"Answer the contents of the receiver, up to the next terminator character. Doubled terminators indicate an embedded terminator character."
	| terminator out ch |
	terminator := $!.
	out := WriteStream on: (String new).
	self skipSeparators.
	[self atEnd] whileFalse: [
		ch := self next.
		(ch == terminator) ifTrue: [
			self peek == terminator ifTrue: [
				self next.  "skip doubled terminator"
			] ifFalse: [
				^ out contents
			].
		].
		out nextPut: ch.
	].
	^ out contents.
%
category: '*core-squeak'
method: WriteStream
nextStringPut: s 
	"Append the string, s, to the receiver.  Only used by DataStream.  Max size of 64*256*256*256."

	| length |
	(length := s size) < 192
		ifTrue: [self nextPut: length]
		ifFalse: 
			[self nextPut: (length digitAt: 4)+192.
			self nextPut: (length digitAt: 3).
			self nextPut: (length digitAt: 2).
			self nextPut: (length digitAt: 1)].
	self nextPutAll: s asByteArray.
	^s
%
category: '*core-squeak'
method: WriteStream
nextLittleEndianNumber: n put: value
	| bytes |
	bytes := ByteArray new: n.
	1 to: n do: [: i | bytes at: i put: (value digitAt: i)].
	self nextPutAll: bytes
%
category: '*core-squeak'
method: WriteStream
store: anObject 
	"Have anObject print on the receiver for purposes of rereading."

	anObject squeakStoreOn: self
%
category: '*core-squeak'
method: WriteStream
nextInt32Put: int32
	"Write a signed integer to the next 4 bytes"
	| pos |
	pos := int32 < 0
		ifTrue: [(0-int32) bitInvert32 + 1]
		ifFalse: [int32].
	1 to: 4 do: [:i | self nextPut: (pos digitAt: 5-i)].
	^ int32
%
category: '*core-squeak'
method: WriteStream
nextNumber: n put: v 
    "Append to the receiver the argument, v, which is a positive 
    Integer, as the next n bytes.
    Possibly pad with leading zeros."

    v < 0 ifTrue:[ self error signal:'expected a positive Integer'].
    1 to: n do: [:i | self nextPut: (v digitAt: n+1-i)].
    ^ v
%
category: '*core-squeak'
method: UndefinedObject
ifNil: aBlock
	"A convenient test, in conjunction with Object ifNil:"

	^ aBlock value
%
category: '*core-squeak'
method: UndefinedObject
ifNotNil: aBlock
	"A convenient test, in conjunction with Object ifNotNil:"

	^ self
%
category: '*core-squeak'
method: BlockClosure
whileFalse
	"Evaluate the receiver once and then repeatedly as long as the value
	returned by the evaluation is true."

	^[self value] whileFalse: []
%
category: '*core-squeak'
method: BlockClosure
ifError: errorHandlerBlock
	"Evaluate the block represented by the receiver, and normally return it's value.  If an error occurs, the errorHandlerBlock is evaluated, and it's value is instead returned.  The errorHandlerBlock must accept zero, one, or two parameters (the error message and the receiver)."
	"Examples:
		[1 whatsUpDoc] ifError: [:err :rcvr | 'huh?'].
		[1 / 0] ifError: [:err :rcvr |
			'ZeroDivide' = err
				ifTrue: [Float infinity]
				ifFalse: [self error: err]]
"

	^ self on: Error do: [:ex |
		errorHandlerBlock valueWithPossibleArgs: #[ex description]]
%
category: '*core-squeak'
method: BlockClosure
valueWithPossibleArgs: anArray

  | n arrSiz newArr |
  (n := self numArgs) == 0 ifTrue: [^ self value].
  n == (arrSiz := anArray size) ifTrue: [^ self valueWithArguments: anArray].
  newArr := Array new: n .
  anArray copyFrom: 1 to: (n > arrSiz ifTrue:[ arrSiz ] ifFalse:[ n ])
	    into: newArr startingAt: 1.  
  ^ self valueWithArguments: newArr
%
category: '*core-squeak'
method: BlockClosure
repeat
	"Evaluate the receiver repeatedly, ending only if the block
	forces some stopping condition."

	[true] whileTrue: [self value]
%
category: '*core-squeak'
method: BlockClosure
whileTrue
	"Evaluate the receiver once and then repeatedly as long as the value
	returned by the evaluation is true."

	^[self value] whileTrue: []
%
doit
Object subclass: 'Archive'
	instVarNames: #( members)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #[]
	inDictionary: ''
	category: 'Core-Squeak'.
true
%
doit
Archive subclass: 'ZipArchive'
	instVarNames: #( centralDirectorySize centralDirectoryOffsetWRTStartingDiskNumber zipFileComment
	                  writeCentralDirectoryOffset writeEOCDOffset)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #[ZipFileConstants _classVars]
	inDictionary: ''
	category: 'Core-Squeak'.
true
%
doit
Object subclass: 'ArchiveMember'
	instVarNames: #( fileName isCorrupt)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #[]
	inDictionary: ''
	category: 'Core-Squeak'.
true
%
doit
ArchiveMember subclass: 'ZipArchiveMember'
	instVarNames: #( lastModFileDateTime fileAttributeFormat versionMadeBy
	                  versionNeededToExtract bitFlag compressionMethod desiredCompressionMethod
	                  desiredCompressionLevel internalFileAttributes externalFileAttributes cdExtraField
	                  localExtraField fileComment crc32 compressedSize
	                  uncompressedSize writeLocalHeaderRelativeOffset readDataRemaining)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #[ZipFileConstants _classVars]
	inDictionary: ''
	category: 'Core-Squeak'.
true
%
doit
ZipArchiveMember subclass: 'ZipFileMember'
	instVarNames: #( externalFileName stream localHeaderRelativeOffset
	                  dataOffset)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #[ZipFileConstants _classVars]
	inDictionary: ''
	category: 'Core-Squeak'.
true
%
doit
ZipArchiveMember subclass: 'ZipNewFileMember'
	instVarNames: #( externalFileName stream)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #[ZipFileConstants _classVars]
	inDictionary: ''
	category: 'Core-Squeak'.
true
%
doit
ZipArchiveMember subclass: 'ZipStringMember'
	instVarNames: #( contents stream)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #[ZipFileConstants _classVars]
	inDictionary: ''
	category: 'Core-Squeak'.
true
%
doit
ByteArray subclass: 'UUID'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #[]
	inDictionary: ''
	category: 'Core-Squeak'.
true
%
doit
Object subclass: 'DiskProxy'
	instVarNames: #( globalObjectName preSelector constructorSelector
	                  constructorArgs)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #[]
	inDictionary: ''
	category: 'Core-Squeak'.
true
%
doit
Error subclass: 'CRCError'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #[]
	inDictionary: ''
	category: 'Core-Squeak'.
true
%
doit
Object subclass: 'GLASSBootstrapDriver'
	instVarNames: #( bootStrapSymbolDictionary repositoryDirectory useRepositoryDirectory
	                  configurationClass configurationPackageLoads configurationVersionString configurationPackageName
	                  configurationRepository metacelloVersionString coreFilenames monticelloFilenames
	                  metacelloFilenames repositoryMap)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #[]
	inDictionary: ''
	category: 'Core-Bootstrap'.
true
%
doit
Object subclass: 'MczInstaller'
	instVarNames: #( stream zip)
	classVars: #( Versions)
	classInstVars: #()
	poolDictionaries: #[]
	inDictionary: ''
	category: 'Core-MCInstaller'.
true
%
doit
Object subclass: 'Scanner'
	instVarNames: #( source mark hereChar
	                  aheadChar token tokenType currentComment
	                  buffer typeTable)
	classVars: #( TypeTable)
	classInstVars: #()
	poolDictionaries: #[]
	inDictionary: ''
	category: 'Core-Squeak'.
true
%
doit
ReadStream subclass: 'ZipReadStream'
	instVarNames: #( readLimit state bitBuf
	                  bitPos source sourcePos sourceLimit
	                  litTable distTable sourceStream crc
	                  expectedCrc)
	classVars: #( BlockProceedBit BlockTypes DistanceMap FixedDistCodes FixedDistTable FixedLitCodes FixedLitTable LiteralLengthMap MaxBits StateNewBlock StateNoMoreData)
	classInstVars: #()
	poolDictionaries: #[]
	inDictionary: ''
	category: 'Core-Squeak'.
true
%
doit
WriteStream subclass: 'ReadWriteStream'
	instVarNames: #()
	classVars: #()
	classInstVars: #()
	poolDictionaries: #[]
	inDictionary: ''
	category: 'Core-Squeak'.
true
%
doit
ReadWriteStream subclass: 'RWBinaryOrTextStream'
	instVarNames: #( isBinary name)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #[]
	inDictionary: ''
	category: 'Core-Squeak'.
true
%
doit
RWBinaryOrTextStream subclass: 'DataStream'
	instVarNames: #( byteStream topCall basePos)
	classVars: #( TypeMap)
	classInstVars: #()
	poolDictionaries: #[]
	inDictionary: ''
	category: 'Core-Squeak'.
true
%
doit
DataStream subclass: 'ReferenceStream'
	instVarNames: #( references objects currentReference
	                  fwdRefEnds blockers skipping insideASegment)
	classVars: #( RefTypes)
	classInstVars: #()
	poolDictionaries: #[]
	inDictionary: ''
	category: 'Core-Squeak'.
true
%
doit
WriteStream subclass: 'ZipEncoder'
	instVarNames: #( bitBuffer bitPosition encodedStream
	                  isBinary)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #[ZipConstants _classVars]
	inDictionary: ''
	category: 'Core-Squeak'.
true
%
doit
WriteStream subclass: 'ZipWriteStream'
	instVarNames: #( writeLimit hashHead hashTail
	                  hashValue blockPosition blockStart literals
	                  distances literalFreq distanceFreq litCount
	                  matchCount encoder crc crcPosition
	                  bytesWritten)
	classVars: #( CrcTable VerboseLevel)
	classInstVars: #()
	poolDictionaries: #[ZipConstants _classVars]
	inDictionary: ''
	category: 'Core-Squeak'.
true
%
doit
Object subclass: 'SystemOrganizer'
	instVarNames: #( categoryDict)
	classVars: #( Default)
	classInstVars: #()
	poolDictionaries: #[]
	inDictionary: ''
	category: 'Core-Squeak'.
true
%

! Remove existing behavior from Archive
removeallmethods Archive
removeallclassmethods Archive
! ------------------- Class methods for Archive
category: 'instance creation'
classmethod: Archive
new

	^(self basicNew) initialize
%
! ------------------- Instance methods for Archive
category: 'archive operations'
method: Archive
addFile: aFileName as: anotherFileName
	| newMember |
	newMember := self memberClass newFromFile: aFileName.
	self addMember: newMember.
	newMember localFileName: anotherFileName.	
	^newMember
%
category: 'archive operations'
method: Archive
addMember: aMember
	^members addLast: aMember
%
category: 'archive operations'
method: Archive
addString: aString as: aFileName
	| newMember |
	newMember := self memberClass newFromString: aString named: aFileName.
	self addMember: newMember.
	newMember localFileName: aFileName.
	^newMember
%
category: 'archive operations'
method: Archive
contentsOf: aMemberOrName
	| member |
	member := self member: aMemberOrName.
	member ifNil: [ ^nil ].
	^member contents
%
category: 'initialization'
method: Archive
initialize
	members := OrderedCollection new.
%
category: 'private'
method: Archive
member: aMemberOrName
	^(members includes: aMemberOrName)
		ifTrue: [ aMemberOrName ]
		ifFalse: [ self memberNamed: aMemberOrName ].
%
category: 'archive operations'
method: Archive
memberNamed: aString
	"Return the first member whose zip name or local file name matches aString, or nil"
	^members detect: [ :ea | ea fileName = aString or: [ ea localFileName = aString ]] ifNone: [ ]
%
category: 'archive operations'
method: Archive
membersMatching: aString
	^members select: [ :ea | (aString match: ea fileName) or: [ aString match: ea localFileName ] ]
%

! Remove existing behavior from ZipArchive
removeallmethods ZipArchive
removeallclassmethods ZipArchive
! ------------------- Class methods for ZipArchive
category: 'constants'
classmethod: ZipArchive
compressionDeflated
	^CompressionDeflated
%
category: 'constants'
classmethod: ZipArchive
findEndOfCentralDirectoryFrom: stream
	"Seek in the given stream to the end, then read backwards until we find the
	signature of the central directory record. Leave the file positioned right
	before the signature.

	Answers the file position of the EOCD, or 0 if not found."

	| data fileLength seekOffset pos maxOffset |
	stream setToEnd.
	fileLength := stream position - 1.
	"If the file length is less than 18 for the EOCD length plus 4 for the signature, we have a problem"
	fileLength < 22 ifTrue: [^ self error: 'file is too short'].
	
	seekOffset := 0.
	pos := 0.
	data := ByteArray new: 4100.
	maxOffset := 40960 min: fileLength.	"limit search range to 40K"

	[
		seekOffset := (seekOffset + 4096) min: fileLength.
		stream position: fileLength - seekOffset + 1.
		data := stream next: (4100 min: seekOffset) into: data startingAt: 1.
		pos := data lastIndexOfPKSignature: EndOfCentralDirectorySignature.
		pos = 0 and: [seekOffset < maxOffset]
	] whileTrue.

	^ pos > 0
		ifTrue: [ | newPos | stream position: (newPos := (stream position + pos - seekOffset - 1)). newPos]
		ifFalse: [0]
%
! ------------------- Instance methods for ZipArchive
category: 'initialization'
method: ZipArchive
initialize
	super initialize.
	writeEOCDOffset := writeCentralDirectoryOffset := 0.
	zipFileComment := ''.
%
category: 'private'
method: ZipArchive
memberClass

	^ZipArchiveMember
%
category: 'private'
method: ZipArchive
readEndOfCentralDirectoryFrom: aStream
	"Read EOCD, starting from position before signature."
	| signature zipFileCommentLength |
	signature := self readSignatureFrom: aStream.
	signature = EndOfCentralDirectorySignature ifFalse: [ ^self error: 'bad signature at ', aStream position printString ].

	aStream nextLittleEndianNumber: 2. "# of this disk"
	aStream nextLittleEndianNumber: 2. "# of disk with central dir start"
	aStream nextLittleEndianNumber: 2. "# of entries in central dir on this disk"
	aStream nextLittleEndianNumber: 2. "total # of entries in central dir"
	centralDirectorySize := aStream nextLittleEndianNumber: 4. "size of central directory"
	centralDirectoryOffsetWRTStartingDiskNumber := aStream nextLittleEndianNumber: 4. "offset of start of central directory"
	zipFileCommentLength := aStream nextLittleEndianNumber: 2. "zip file comment"
	zipFileComment := aStream next: zipFileCommentLength.
%
category: 'reading'
method: ZipArchive
readFrom: aStreamOrFileName
	| stream name eocdPosition |
	stream := aStreamOrFileName isStream
		ifTrue: [
			name := aStreamOrFileName name. 
			aStreamOrFileName]
		ifFalse: [self error: 'from fileName not implemented yet'].
	stream binary.
	eocdPosition := self class findEndOfCentralDirectoryFrom: stream.
	eocdPosition <= 0 ifTrue: [self error: 'can''t find EOCD position'].
	self readEndOfCentralDirectoryFrom: stream.
	stream position: eocdPosition - centralDirectorySize.
	self readMembersFrom: stream named: name
%
category: 'private'
method: ZipArchive
readMembersFrom: stream named: fileName
	| newMember signature |
	[
		newMember := self memberClass newFromZipFile: stream named: fileName.
		signature := self readSignatureFrom: stream.
		signature = EndOfCentralDirectorySignature ifTrue: [ ^self ].
		signature = CentralDirectoryFileHeaderSignature
			ifFalse: [ self error: 'bad CD signature at ', (stream position - 4) hex ].
		newMember readFrom: stream.
		"newMember looksLikeDirectory ifTrue: [ newMember := newMember asDirectory ]." "ZipFileDirectory not supported"
		self addMember: newMember.
	] repeat.
%
category: 'private'
method: ZipArchive
readSignatureFrom: stream
	"Returns next signature from given stream, leaves stream positioned afterwards."

	| signatureData | 
	signatureData := ByteArray new: 4.
	stream next: 4 into: signatureData.
	(#[ CentralDirectoryFileHeaderSignature, LocalFileHeaderSignature, EndOfCentralDirectorySignature ]
		includes: signatureData)
			ifFalse: [ ^self error: 'bad signature ', signatureData asString asHex, ' at position ', (stream position - 4) asString ].
	^signatureData
%
category: 'writing'
method: ZipArchive
writeCentralDirectoryTo: aStream
	| offset |
	offset := writeCentralDirectoryOffset.
	members do: [ :member |
		member writeCentralDirectoryFileHeaderTo: aStream.
		offset := offset + member centralDirectoryHeaderSize.
	].
	writeEOCDOffset := offset.
	self writeEndOfCentralDirectoryTo: aStream.
%
category: 'private'
method: ZipArchive
writeEndOfCentralDirectoryTo: aStream

	aStream nextPutAll: EndOfCentralDirectorySignature.
	aStream nextLittleEndianNumber: 2 put: 0. "diskNumber"
	aStream nextLittleEndianNumber: 2 put: 0. "diskNumberWithStartOfCentralDirectory"
	aStream nextLittleEndianNumber: 2 put: members size. "numberOfCentralDirectoriesOnThisDisk"
	aStream nextLittleEndianNumber: 2 put: members size. "numberOfCentralDirectories"
	aStream nextLittleEndianNumber: 4 put: writeEOCDOffset - writeCentralDirectoryOffset. "size of central dir"
	aStream nextLittleEndianNumber: 4 put: writeCentralDirectoryOffset. "offset of central dir"
	aStream nextLittleEndianNumber: 2 put: zipFileComment size. "zip file comment"
	zipFileComment isEmpty ifFalse: [ aStream nextPutAll: zipFileComment ].
%
category: 'writing'
method: ZipArchive
writeTo: stream
	members do: [ :member |
		member writeTo: stream.
		member endRead.
	].
	writeCentralDirectoryOffset := stream position.
	self writeCentralDirectoryTo: stream.
	
%

! Remove existing behavior from ArchiveMember
removeallmethods ArchiveMember
removeallclassmethods ArchiveMember
! ------------------- Class methods for ArchiveMember
category: 'instance creation'
classmethod: ArchiveMember
new

	^(self basicNew) initialize
%
category: 'instance creation'
classmethod: ArchiveMember
newFromFile: aFileName

	self subclassResponsibility
%
! ------------------- Instance methods for ArchiveMember
category: 'accessing'
method: ArchiveMember
fileName
	^fileName
%
category: 'initialization'
method: ArchiveMember
initialize
	fileName := ''.
	isCorrupt := false.
%
category: 'accessing'
method: ArchiveMember
isCorrupt: aBoolean
	"Mark this member as being corrupt."
	isCorrupt := aBoolean
%
category: 'accessing'
method: ArchiveMember
localFileName: aString
	"Set my internal filename.
	Returns the (possibly new) filename.
	aString will be translated from local FS format into Unix format."

	^fileName := aString "copyReplaceAll: FileDirectory slash with: '/'."
%

! Remove existing behavior from ZipArchiveMember
removeallmethods ZipArchiveMember
removeallclassmethods ZipArchiveMember
! ------------------- Class methods for ZipArchiveMember
category: 'instance creation'
classmethod: ZipArchiveMember
newFromString: aString named: aFileName

	^ZipStringMember newFrom: aString named: aFileName
%
category: 'instance creation'
classmethod: ZipArchiveMember
newFromZipFile: stream named: fileName
	^ZipFileMember newFrom: stream named: fileName
%
! ------------------- Instance methods for ZipArchiveMember
category: 'accessing'
method: ZipArchiveMember
centralDirectoryHeaderSize

	| systemFileName systemFileComment systemCdExtraField |
	systemFileName := fileName.
	systemFileComment := fileComment.
	systemCdExtraField := cdExtraField.
	^ 46 + systemFileName size + systemCdExtraField size + systemFileComment size
%
category: 'private'
method: ZipArchiveMember
compressDataTo: aStream
	"Copy my deflated data to the given stream."
	| startPos endPos compressedBytes unCompressedBytes |

	startPos := aStream position.

	unCompressedBytes := (self readRawChunk: uncompressedSize) asByteArray.
	compressedBytes :=  unCompressedBytes _compressBytes.
	compressedBytes == nil 
		ifTrue: [ 
			"needed to undo the previous readRawChunk:"
			self rewindData.
			^self stCompressDataTo: aStream ].
	aStream nextPutAll: compressedBytes asString.
	endPos := aStream position.
	compressedSize := endPos - startPos.
	crc32 := unCompressedBytes _computeCRC32: 0.
%
category: 'reading'
method: ZipArchiveMember
contents
	"Answer my contents as a string."
	| s |
	s := RWBinaryOrTextStream on: String new.
	self extractTo: s.
	s text.
	^s contents
%
category: 'accessing'
method: ZipArchiveMember
contentStream
	"Answer my contents as a text stream.
	Default is no conversion, since we don't know what the bytes mean."

	| s |
	s := RWBinaryOrTextStream on: (String new: self uncompressedSize).
	self extractTo: s.
	s reset.
	^ s.
%
category: 'private'
method: ZipArchiveMember
copyRawDataTo: aStream

	[ readDataRemaining > 0 ] whileTrue: [ | data |
		data := self readRawChunk: (4096 min: readDataRemaining).
		aStream nextPutAll: data.
		readDataRemaining := readDataRemaining - data size.
	].
%
category: 'accessing'
method: ZipArchiveMember
crc32
	^crc32
%
category: 'accessing'
method: ZipArchiveMember
desiredCompressionMethod: aNumber
	"Set my desiredCompressionMethod
	This is the method that will be used to write.
	Answers prior desiredCompressionMethod.

	Only CompressionDeflated or CompressionStored are valid arguments.

	Changing to CompressionStored will change my desiredCompressionLevel
	to CompressionLevelNone; changing to CompressionDeflated will change my
	desiredCompressionLevel to CompressionLevelDefault."

	| old |
	old := desiredCompressionMethod.
	desiredCompressionMethod := aNumber.
	desiredCompressionLevel := (aNumber = CompressionDeflated)
			ifTrue: [ CompressionLevelDefault ]
			ifFalse: [ CompressionLevelNone ].
	compressionMethod = CompressionStored ifTrue: [ compressedSize := uncompressedSize ].
	^old.
%
category: 'private'
method: ZipArchiveMember
endRead
	readDataRemaining := 0.
%
category: 'extraction'
method: ZipArchiveMember
extractTo: aStream
	| oldCompression |
	self isEncrypted ifTrue: [ self error: 'encryption is unsupported' ].
	aStream binary.
	oldCompression := self desiredCompressionMethod: CompressionStored.
	self rewindData.
	self writeDataTo: aStream.
	self desiredCompressionMethod: oldCompression.
	self endRead.
%
category: 'testing'
method: ZipArchiveMember
hasDataDescriptor
	^ (bitFlag bitAnd: 8)	~= 0 "GPBF_HAS_DATA_DESCRIPTOR_MASK"
%
category: 'initialization'
method: ZipArchiveMember
initialize
	super initialize.
	lastModFileDateTime := 0.
	fileAttributeFormat := FaUnix.
	versionMadeBy := 20.
	versionNeededToExtract := 20.
	bitFlag := 0.
	compressionMethod := CompressionStored.
	desiredCompressionMethod := CompressionDeflated.
	desiredCompressionLevel := CompressionLevelDefault.
	internalFileAttributes := 0.
	externalFileAttributes := 0.
	fileName := ''.
	cdExtraField := ''.
	localExtraField := ''.
	fileComment := ''.
	crc32 := 0.
	compressedSize := 0.
	uncompressedSize := 0.
	self unixFileAttributes: DefaultFilePermissions.
%
category: 'testing'
method: ZipArchiveMember
isDirectory
	^false
%
category: 'testing'
method: ZipArchiveMember
isEncrypted
	"Return true if this member is encrypted (this is unsupported)"
	^ (bitFlag bitAnd: 1) ~= 0
%
category: 'accessing'
method: ZipArchiveMember
localFileName
	"Answer my fileName in terms of the local directory naming convention"
	"| localName |
	localName := fileName copyReplaceAll: '/' with: FileDirectory slash.
	^(fileName first = $/)
		ifTrue: [ FileDirectory makeAbsolute: localName ]
		ifFalse: [ FileDirectory makeRelative: localName ]"
	^fileName
%
category: 'testing'
method: ZipArchiveMember
looksLikeDirectory
	^false
%
category: 'private'
method: ZipArchiveMember
mapPermissionsFromUnix: unixPerms
	^ unixPerms bitShift: 16.
%
category: 'private'
method: ZipArchiveMember
mapPermissionsToUnix: dosPerms
	^ dosPerms bitShift: -16.
%
category: 'private'
method: ZipArchiveMember
refreshLocalFileHeaderTo: aStream
	"Re-writes my local header to the given stream.
	To be called after writing the data stream.
	Assumes that fileName and localExtraField sizes didn't change since last written."

	| here systemFileName |
	here := aStream position.
	systemFileName := fileName.
	aStream position: writeLocalHeaderRelativeOffset.

	aStream nextPutAll: LocalFileHeaderSignature.
	aStream nextLittleEndianNumber: 2 put: versionNeededToExtract.
	aStream nextLittleEndianNumber: 2 put: bitFlag.
	aStream nextLittleEndianNumber: 2 put: desiredCompressionMethod.
	aStream nextLittleEndianNumber: 4 put: lastModFileDateTime.
	aStream nextLittleEndianNumber: 4 put: crc32.
	aStream nextLittleEndianNumber: 4 put: (desiredCompressionMethod = CompressionStored
												ifTrue: [ uncompressedSize ] ifFalse: [ compressedSize ]).
	aStream nextLittleEndianNumber: 4 put: uncompressedSize.
	aStream nextLittleEndianNumber: 2 put: systemFileName size.
	aStream nextLittleEndianNumber: 2 put: localExtraField size.

	aStream position: here.
%
category: 'private'
method: ZipArchiveMember
rewindData
	readDataRemaining :=  (desiredCompressionMethod = CompressionDeflated
		and: [ compressionMethod = CompressionDeflated ])
			ifTrue: [ compressedSize ]
			ifFalse: [ uncompressedSize ].
%
category: 'accessing'
method: ZipArchiveMember
setLastModFileDateTimeFrom: aSmalltalkTime
	| unixTime |
	unixTime := aSmalltalkTime -  2177424000.		"PST?"
	lastModFileDateTime := self unixToDosTime: unixTime
%
category: 'private'
method: ZipArchiveMember
stCompressDataTo: aStream
	"Copy my deflated data to the given stream."
	| encoder startPos endPos |

	encoder := ZipWriteStream on: aStream.
	startPos := aStream position.

	[ readDataRemaining > 0 ] whileTrue: [ | data |
		data := self readRawChunk: (4096 min: readDataRemaining).
		encoder nextPutAll: data asString.
		readDataRemaining := readDataRemaining - data size.
	].
	encoder finish. "not close!"
	endPos := aStream position.
	compressedSize := endPos - startPos.
	crc32 := encoder crc.
%
category: 'accessing'
method: ZipArchiveMember
uncompressedSize
	"Return the uncompressed size for this member."
	^uncompressedSize
%
category: 'accessing'
method: ZipArchiveMember
unixFileAttributes: perms
	| oldPerms newPerms |
	oldPerms := self mapPermissionsToUnix: externalFileAttributes.
	newPerms :=  self isDirectory
			ifTrue: [ (perms bitAnd: FileAttrib bitInvert) bitOr: DirectoryAttrib ]
			ifFalse: [ (perms bitAnd: DirectoryAttrib bitInvert) bitOr: FileAttrib ].
	externalFileAttributes := self mapPermissionsFromUnix: newPerms.
	^oldPerms.
%
category: 'private'
method: ZipArchiveMember
unixToDosTime: unixTime
	| dosTime dateTime secs |
	secs := self unixToSqueakTime: unixTime.	"Squeak time (PST?)"
	dateTime := Time dateAndTimeFromSeconds: secs.
	dosTime := (dateTime second seconds) bitShift: -1.
	dosTime := dosTime + ((dateTime second minutes) bitShift: 5).
	dosTime := dosTime + ((dateTime second hours) bitShift: 11).
	dosTime := dosTime + ((dateTime first dayOfMonth) bitShift: 16).
	dosTime := dosTime + ((dateTime first monthIndex) bitShift: 21).
	dosTime := dosTime + (((dateTime first year) - 1980) bitShift: 25).
	^dosTime
%
category: 'private'
method: ZipArchiveMember
unixToSqueakTime: unixTime
	^unixTime +  2177424000.		"Squeak time (PST?)"
%
category: 'private'
method: ZipArchiveMember
writeCentralDirectoryFileHeaderTo: aStream
	"C2 v3 V4 v5 V2"

	| systemFileName systemFileComment systemCdExtraField |
	systemFileName := fileName.
	systemFileComment := fileComment.
	systemCdExtraField := cdExtraField.
	aStream nextPutAll: CentralDirectoryFileHeaderSignature.
	aStream nextLittleEndianNumber: 1 put: versionMadeBy.
	aStream nextLittleEndianNumber: 1 put: fileAttributeFormat.

	aStream nextLittleEndianNumber: 2 put: versionNeededToExtract.
	aStream nextLittleEndianNumber: 2 put: bitFlag.
	aStream nextLittleEndianNumber: 2 put: desiredCompressionMethod.

	aStream nextLittleEndianNumber: 4 put: lastModFileDateTime.

	"These next 3 should have been updated during the write of the data"
	aStream nextLittleEndianNumber: 4 put: crc32.
	aStream nextLittleEndianNumber: 4 put: (desiredCompressionMethod = CompressionStored
												ifTrue: [ uncompressedSize ] ifFalse: [ compressedSize ]).
	aStream nextLittleEndianNumber: 4 put: uncompressedSize.

	aStream nextLittleEndianNumber: 2 put: systemFileName size.
	aStream nextLittleEndianNumber: 2 put: systemCdExtraField size.
	aStream nextLittleEndianNumber: 2 put: systemFileComment size.
	aStream nextLittleEndianNumber: 2 put: 0.		"diskNumberStart"
	aStream nextLittleEndianNumber: 2 put: internalFileAttributes.

	aStream nextLittleEndianNumber: 4 put: externalFileAttributes.
	aStream nextLittleEndianNumber: 4 put: (writeLocalHeaderRelativeOffset - 1).

	aStream nextPutAll: systemFileName asByteArray.
	aStream nextPutAll: systemCdExtraField asByteArray.
	aStream nextPutAll: systemFileComment asByteArray.
%
category: 'private'
method: ZipArchiveMember
writeDataTo: aStream
	"Copy my (possibly inflated or deflated) data to the given stream.
	This might do compression, decompression, or straight copying, depending
	on the values of compressionMethod and desiredCompressionMethod"

	uncompressedSize = 0 ifTrue: [ ^self ].	"nothing to do because no data"

	(compressionMethod = CompressionStored and: [ desiredCompressionMethod = CompressionDeflated ])
		ifTrue: [ ^self compressDataTo: aStream ].

	(compressionMethod = CompressionDeflated and: [ desiredCompressionMethod = CompressionStored ])
		ifTrue: [ ^self uncompressDataTo: aStream ].

	self copyDataTo: aStream.
%
category: 'private'
method: ZipArchiveMember
writeLocalFileHeaderTo: aStream
	"Write my local header to a file handle.
	Stores the offset to the start of the header in my
	writeLocalHeaderRelativeOffset member."

	| systemFileName |
	systemFileName := fileName.
	aStream nextPutAll: LocalFileHeaderSignature.
	aStream nextLittleEndianNumber: 2 put: versionNeededToExtract.
	aStream nextLittleEndianNumber: 2 put: bitFlag.
	aStream nextLittleEndianNumber: 2 put: desiredCompressionMethod.

	aStream nextLittleEndianNumber: 4 put: lastModFileDateTime.
	aStream nextLittleEndianNumber: 4 put: crc32.
	aStream nextLittleEndianNumber: 4 put: (desiredCompressionMethod = CompressionStored
												ifTrue: [ uncompressedSize ] ifFalse: [ compressedSize ]).
	aStream nextLittleEndianNumber: 4 put: uncompressedSize.

	aStream nextLittleEndianNumber: 2 put: systemFileName size.
	aStream nextLittleEndianNumber: 2 put: localExtraField size.

	aStream nextPutAll: systemFileName asByteArray.
	aStream nextPutAll: localExtraField asByteArray.
%
category: 'writing'
method: ZipArchiveMember
writeTo: aStream
	self rewindData.
	writeLocalHeaderRelativeOffset := aStream position.
	self writeLocalFileHeaderTo: aStream.
	self writeDataTo: aStream.
	self refreshLocalFileHeaderTo: aStream.
%

! Remove existing behavior from ZipFileMember
removeallmethods ZipFileMember
removeallclassmethods ZipFileMember
! ------------------- Class methods for ZipFileMember
category: 'instance creation'
classmethod: ZipFileMember
newFrom: stream named: fileName
	^(self new) initialize stream: stream externalFileName: fileName
%
! ------------------- Instance methods for ZipFileMember
category: 'private'
method: ZipFileMember
canonicalizeFileName
	"For security reasons, make all paths relative and remove any ../ portions"

	[fileName beginsWith: '/'] whileTrue: [fileName := fileName allButFirst].
	fileName := fileName copyReplaceAll: '../' with: ''
%
category: 'writing'
method: ZipFileMember
copyDataTo: aStream

	self copyRawDataTo: aStream.
%
category: 'initialize-release'
method: ZipFileMember
initialize
	super initialize.
	crc32 := 0.
	localHeaderRelativeOffset := 0.
	dataOffset := 0.
%
category: 'testing'
method: ZipFileMember
looksLikeDirectory
	^fileName last = $/
		and: [ uncompressedSize = 0 ]
%
category: 'private'
method: ZipFileMember
readCentralDirectoryFileHeaderFrom: aStream
	"Assumes aStream positioned after signature"

	| fileNameLength extraFieldLength fileCommentLength |

	versionMadeBy := aStream nextLittleEndianNumber: 1.
	fileAttributeFormat := aStream nextLittleEndianNumber: 1.

	versionNeededToExtract := aStream nextLittleEndianNumber: 2.
	bitFlag := aStream nextLittleEndianNumber: 2.
	compressionMethod := aStream nextLittleEndianNumber: 2.

	lastModFileDateTime := aStream nextLittleEndianNumber: 4.
	crc32 := aStream nextLittleEndianNumber: 4.
	compressedSize := aStream nextLittleEndianNumber: 4.
	uncompressedSize := aStream nextLittleEndianNumber: 4.

	fileNameLength := aStream nextLittleEndianNumber: 2.
	extraFieldLength := aStream nextLittleEndianNumber: 2.
	fileCommentLength := aStream nextLittleEndianNumber: 2.
	aStream nextLittleEndianNumber: 2. 	"disk number start"
	internalFileAttributes := aStream nextLittleEndianNumber: 2.

	externalFileAttributes := aStream nextLittleEndianNumber: 4.
	localHeaderRelativeOffset := aStream nextLittleEndianNumber: 4.

	fileName := (aStream next: fileNameLength) asString.
	cdExtraField := (aStream next: extraFieldLength) asByteArray asString.
	fileComment := (aStream next: fileCommentLength) asString.

	self desiredCompressionMethod: compressionMethod
%
category: 'private'
method: ZipFileMember
readFrom: aStream 
	"assumes aStream positioned after CD header; leaves stream positioned after my CD entry"

	self readCentralDirectoryFileHeaderFrom: aStream.
	self readLocalDirectoryFileHeaderFrom: aStream.
	self endRead.
	self canonicalizeFileName.
%
category: 'private'
method: ZipFileMember
readLocalDirectoryFileHeaderFrom: aStream 
	"Positions stream as necessary. Will return stream to its original position"

	| fileNameLength extraFieldLength xcrc32 xcompressedSize xuncompressedSize sig oldPos |

	oldPos := aStream position.

	aStream position: localHeaderRelativeOffset + 1.

	sig := aStream next: 4.
	sig = LocalFileHeaderSignature asByteArray
		ifFalse: [ aStream position: oldPos.
				^self error: 'bad LH signature at ', localHeaderRelativeOffset hex ].

	versionNeededToExtract := aStream nextLittleEndianNumber: 2.
	bitFlag := aStream nextLittleEndianNumber: 2.
	compressionMethod := aStream nextLittleEndianNumber: 2.

	lastModFileDateTime := aStream nextLittleEndianNumber: 4.
	xcrc32 := aStream nextLittleEndianNumber: 4.
	xcompressedSize := aStream nextLittleEndianNumber: 4.
	xuncompressedSize := aStream nextLittleEndianNumber: 4.

	fileNameLength := aStream nextLittleEndianNumber: 2.
	extraFieldLength := aStream nextLittleEndianNumber: 2.

	fileName := (aStream next: fileNameLength) asString.
	localExtraField := (aStream next: extraFieldLength) asByteArray.

	dataOffset := aStream position.

	"Don't trash these fields if we already got them from the central directory"
	self hasDataDescriptor ifFalse: [
		crc32 := xcrc32.
		compressedSize := xcompressedSize.
		uncompressedSize := xuncompressedSize.
	].

	aStream position: oldPos.
%
category: 'private'
method: ZipFileMember
readRawChunk: n
	^stream next: n
%
category: 'private'
method: ZipFileMember
rewindData
	super rewindData.
	(stream isNil or: [ stream closed ])
		ifTrue: [ self error: 'stream missing or closed' ].
	stream position: (localHeaderRelativeOffset + 4 + 1).
	self skipLocalDirectoryFileHeaderFrom: stream.
%
category: 'private'
method: ZipFileMember
skipLocalDirectoryFileHeaderFrom: aStream 
	"Assumes that stream is positioned after signature."

	|  extraFieldLength fileNameLength |
	aStream next: 22.
	fileNameLength := aStream nextLittleEndianNumber: 2.
	extraFieldLength := aStream nextLittleEndianNumber: 2.
	aStream next: fileNameLength.
	aStream next: extraFieldLength.
	dataOffset := aStream position.
%
category: 'initialize-release'
method: ZipFileMember
stream: aStream externalFileName: aFileName
	stream := aStream.
	externalFileName := aFileName.
%
category: 'private'
method: ZipFileMember
stUncompressDataTo: aStream

	| decoder buffer chunkSize crcErrorMessage |
	decoder := ZipReadStream on: stream.
	decoder expectedCrc: self crc32.
	buffer := ByteArray new: (32768 min: readDataRemaining).
	crcErrorMessage := nil.

	[[ readDataRemaining > 0 ] whileTrue: [
		chunkSize := 32768 min: readDataRemaining.
		buffer := decoder next: chunkSize into: buffer startingAt: 1.
		aStream next: chunkSize putAll: buffer startingAt: 1.
		readDataRemaining := readDataRemaining - chunkSize.
	]] on: CRCError do: [ :ex | crcErrorMessage := ex messageText. ex resume ].

	crcErrorMessage ifNotNil: [ self isCorrupt: true. CRCError signal: crcErrorMessage ]
%
category: 'private'
method: ZipFileMember
uncompressDataTo: aStream

	| compressedBytes unCompressedBytes pos | 
	pos := stream position.
	compressedBytes := (stream contents copyFrom: pos to: (pos + compressedSize)) asByteArray.
	unCompressedBytes := compressedBytes _decompressBytes.
	unCompressedBytes == nil ifTrue: [ ^self stUncompressDataTo: aStream ].
	self crc32 ~= (unCompressedBytes _computeCRC32: 0) ifTrue: [ self error: 'Throw CRCError here' ].
	aStream nextPutAll: unCompressedBytes.
%

! Remove existing behavior from ZipNewFileMember
removeallmethods ZipNewFileMember
removeallclassmethods ZipNewFileMember
! ------------------- Class methods for ZipNewFileMember
category: 'instance creation'
classmethod: ZipNewFileMember
newNamed: aFileName
	^(self new) from: aFileName
%
! ------------------- Instance methods for ZipNewFileMember
category: 'initialization'
method: ZipNewFileMember
close
	stream ifNotNil:[stream close].
%
category: 'private'
method: ZipNewFileMember
fileDirectoryClass
	"indirection because this class loaded early on during bootstrapping"

        ^System myUserProfile symbolList objectNamed: #FileDirectory
%
category: 'initialization'
method: ZipNewFileMember
from: aFileName
	compressionMethod := CompressionStored.
	"Now get the size, attributes, and timestamps, and see if the file exists"
	stream := self fileDirectoryClass readOnlyFileNamed: aFileName.
	self localFileName: (externalFileName := stream name).
	compressedSize := uncompressedSize := stream fileSize.
	desiredCompressionMethod := compressedSize > 0 ifTrue: [ CompressionDeflated ] ifFalse: [ CompressionStored ].
	self setLastModFileDateTimeFrom: stream lastModified
%
category: 'initialization'
method: ZipNewFileMember
initialize
	super initialize.
	externalFileName := ''.
%
category: 'private'
method: ZipNewFileMember
readRawChunk: n
	^stream next: n
%
category: 'private-writing'
method: ZipNewFileMember
rewindData
	super rewindData.
	readDataRemaining := stream size.
	stream position: 0.
%
category: 'testing'
method: ZipNewFileMember
usesFileNamed: aFileName
	"Do I require aFileName? That is, do I care if it's clobbered?"
	^(self fileDirectoryClass default fullNameFor: externalFileName) = (self fileDirectoryClass default fullNameFor: aFileName)
%

! Remove existing behavior from ZipStringMember
removeallmethods ZipStringMember
removeallclassmethods ZipStringMember
! ------------------- Class methods for ZipStringMember
category: 'instance creation'
classmethod: ZipStringMember
newFrom: aString named: aFileName
	^(self new) contents: aString; localFileName: aFileName; yourself
%
! ------------------- Instance methods for ZipStringMember
category: 'private'
method: ZipStringMember
contents

	^contents
%
category: 'private'
method: ZipStringMember
contents: aString

	contents := aString.
	compressedSize := uncompressedSize := aString size.
	"set the file date to now"
	self setLastModFileDateTimeFrom: Time totalSeconds
%
category: 'initialization'
method: ZipStringMember
initialize
	super initialize.
	self contents: ''.
	compressionMethod := desiredCompressionMethod := CompressionStored.
%
category: 'private'
method: ZipStringMember
readRawChunk: n
	^stream next: n
%
category: 'initialization'
method: ZipStringMember
rewindData
	super rewindData.
	stream := ReadStream on: contents.
	readDataRemaining := contents size.
%

! Remove existing behavior from UUID
removeallmethods UUID
removeallclassmethods UUID
! ------------------- Class methods for UUID
category: 'instance creation'
classmethod: UUID
fromString: aString
	| object |
	aString size ~= 36 ifTrue: [Error signal].
	object := self nilUUID. 
	object asUUID: aString.
	^object
%
category: 'instance creation'
classmethod: UUID
nilUUID
	"Must call basicNew: here because I have a non-trivial initialize method."

	^self basicNew: 16
%
! ------------------- Instance methods for UUID
category: 'comparing'
method: UUID
<= uuid
	^ (self = uuid) or: [ self < uuid ]
%
category: 'comparing'
method: UUID
> uuid
	^ uuid < self
%
category: 'comparing'
method: UUID
>= uuid
	^ (self = uuid) or: [ uuid < self ]
%
category: 'converting'
method: UUID
asString
	| result data |
	data := String new: 36.
	result := WriteStream on: data.
	1 to: 4 do:[:i| self printHexAt: i to: result].
	result nextPut: $-.
	5 to: 6 do:[:i| self printHexAt: i to: result].
	result nextPut: $-.
	7 to: 8 do:[:i| self printHexAt: i to: result].
	result nextPut: $-.
	9 to: 10 do:[:i| self printHexAt: i to: result].
	result nextPut: $-.
	11 to: 16 do:[:i| self printHexAt: i to: result].
	^data.
	
%
category: 'converting'
method: UUID
asUUID: aString
	| stream token byte sz |
	stream := ReadStream on: (aString copyReplaceAll: '-' with: '') asUppercase.
	sz := stream _collection size.
	1 to: sz/2 do: [:i | 
		token := stream next: 2.
		byte := Integer fromHexString: token.
		self at: i put: byte].
	^self
%
category: 'converting'
method: UUID
printHexAt: index to: aStream
	| map v |
	map := '0123456789abcdef'.
	v := self at: index.
	aStream nextPut: (map at: (v bitShift: -4) + 1). 
	aStream nextPut: (map at: (v bitAnd: 15) + 1).
%
category: 'printing'
method: UUID
printOn: aStream
	aStream nextPutAll: 'an UUID('.
	self asString printOn: aStream.
	aStream nextPutAll: ')'
%
category: 'printing'
method: UUID
printString

	^self asString
%

! Remove existing behavior from DiskProxy
removeallmethods DiskProxy
removeallclassmethods DiskProxy
! ------------------- Class methods for DiskProxy
category: 'instance creation'
classmethod: DiskProxy
global: globalNameSymbol selector: selectorSymbol args: argArray
    "Create a new DiskProxy constructor with the given
     globalNameSymbol, selectorSymbol, and argument Array.
     It will internalize itself by looking up the global object name
     in the SystemDictionary (Smalltalk) and sending it this message
     with these arguments."

    ^ self new global: globalNameSymbol
             selector: selectorSymbol
                 args: argArray
%
! ------------------- Instance methods for DiskProxy
category: 'loading'
method: DiskProxy
comeFullyUpOnReload: smartRefStream
	"Internalize myself into a fully alive object after raw loading from a DataStream. (See my class comment.)  DataStream will substitute the object from this eval for the DiskProxy."
	| globalObj symbol pr nn arrayIndex |

	symbol := globalObjectName.
	"See if class is mapped to another name"
	globalObj := System myUserProfile symbolList objectNamed: symbol.
	globalObj == nil ifTrue: [
		^ self error: 'Global "', symbol, '" not found'].
	preSelector ifNotNil: [
		Symbol hasInterned: preSelector ifTrue: [:selector |
			[globalObj := globalObj perform: selector] on: Error do: [:ex |
				ex messageText = 'key not found' ifTrue: [^ nil].
				^ ex signal]]
	].
	constructorSelector ifNil: [^ globalObj].
	Symbol hasInterned: constructorSelector ifTrue: [:selector |
		[^ globalObj perform: selector withArguments: constructorArgs] on: Error do: [:ex |
			ex messageText = 'key not found' ifTrue: [^ nil].
			^ ex signal]
	].
				"args not checked against Renamed"
	^ nil 	"was not in proper form"
%
category: 'initialization'
method: DiskProxy
global: globalNameSymbol selector: selectorSymbol args: argArray
	"Initialize self as a DiskProxy constructor with the given
	globalNameSymbol, selectorSymbol, and argument Array.
	I will internalize by looking up the global object name in the
	SystemDictionary (Smalltalk) and sending it this message with
	these arguments."

	globalObjectName := globalNameSymbol asSymbol.
	constructorSelector := selectorSymbol asSymbol.
	constructorArgs := argArray.
%

! Remove existing behavior from CRCError
removeallmethods CRCError
removeallclassmethods CRCError
! ------------------- Class methods for CRCError
! ------------------- Instance methods for CRCError

! Remove existing behavior from GLASSBootstrapDriver
removeallmethods GLASSBootstrapDriver
removeallclassmethods GLASSBootstrapDriver
! ------------------- Class methods for GLASSBootstrapDriver
! ------------------- Instance methods for GLASSBootstrapDriver
category: 'auto commit'
method: GLASSBootstrapDriver
autoCommitDuring: aBlock

	| commitThreshold |
	commitThreshold :=  75.
	"Install AlmostOutOfMemory handler"
	Exception
		category: GemStoneError
		number: (ErrorSymbols at: #rtErrSignalAlmostOutOfMemory)
		do: [:ex :cat :num :args |
			"Exception caught, do a commit."
			System commitTransaction 
				ifFalse: [ self error: 'AutoCommit failed' ].
			"run a markSweep"
			System _vmMarkSweep.
			(System _tempObjSpacePercentUsedLastMark < commitThreshold)
				ifTrue: [
					"We dropped below the threshold reenable the signal"
					System enableAlmostOutOfMemoryError ].
			"continue execution" ].
	"Enable AlmostOutOfMemory signal"
	System signalAlmostOutOfMemoryThreshold: commitThreshold.
	aBlock ensure: [
		"disable AlmostOutOfMemory signal"
		System signalAlmostOutOfMemoryThreshold: -1].
%
category: 'accessing'
method: GLASSBootstrapDriver
bootStrapSymbolDictionary

   ^bootStrapSymbolDictionary
%
category: 'accessing'
method: GLASSBootstrapDriver
bootStrapSymbolDictionary: newValue

   bootStrapSymbolDictionary := newValue
%
category: 'accessing'
method: GLASSBootstrapDriver
configurationClass

   ^configurationClass
%
category: 'accessing'
method: GLASSBootstrapDriver
configurationClass: newValue

   configurationClass := newValue
%
category: 'accessing'
method: GLASSBootstrapDriver
configurationPackageLoads

   ^configurationPackageLoads
%
category: 'accessing'
method: GLASSBootstrapDriver
configurationPackageLoads: newValue

  configurationPackageLoads := newValue
%
category: 'accessing'
method: GLASSBootstrapDriver
configurationPackageName

   ^configurationPackageName
%
category: 'accessing'
method: GLASSBootstrapDriver
configurationPackageName: newValue

   configurationPackageName := newValue
%
category: 'accessing'
method: GLASSBootstrapDriver
configurationRepository

   ^configurationRepository
%
category: 'accessing'
method: GLASSBootstrapDriver
configurationRepository: newValue

  configurationRepository := newValue
%
category: 'accessing'
method: GLASSBootstrapDriver
configurationVersionString

   ^configurationVersionString
%
category: 'accessing'
method: GLASSBootstrapDriver
configurationVersionString: newValue

   configurationVersionString := newValue
%
category: 'accessing'
method: GLASSBootstrapDriver
coreFilenames

   ^coreFilenames
%
category: 'accessing'
method: GLASSBootstrapDriver
coreFilenames: newValue

   coreFilenames := newValue
%
category: 'class accessing'
method: GLASSBootstrapDriver
goferPackageReferenceClass

	^System myUserProfile symbolList objectNamed: #GoferPackageReference
%
category: 'installing'
method: GLASSBootstrapDriver
install

	self 
		installMcz;
		installMetacello;
		installConfiguration
%
category: 'installing'
method: GLASSBootstrapDriver
installConfiguration

	self autoCommitDuring: [
		self 
			installProject: self configurationClass 
			version: self configurationVersionString 
			loads: self configurationPackageLoads.

		self repositoryMap associationsDo: [:assoc | | wc |
			wc := [ (self goferPackageReferenceClass name: assoc key) workingCopy ] 
				on: Error 
				do: [:ex | ex return: nil ].
			wc ~~ nil
				ifTrue: [
					wc repositoryGroup addRepository: (self mCHttpRepositoryClass location: assoc value first user: '' password: '') ]].
		System commitTransaction ifFalse: [ ^self error: 'commit failed' ] ].
%
category: 'installing'
method: GLASSBootstrapDriver
installConfigurationPackage

	self autoCommitDuring: [
		self 
			installPackageSpec: self configurationPackageName 
			repository: self configurationRepository.

		System commitTransaction ifFalse: [ ^self error: 'commit failed' ] ].
%
category: 'installing'
method: GLASSBootstrapDriver
installMcz

	"Boot monticello using mcz loader"
	self 
		mczInstall: self coreFilenames silently: false;
		mczInstall: self monticelloFilenames silently: true.
	self postMczInstallInitialization.
%
category: 'installing'
method: GLASSBootstrapDriver
installMetacello

	self autoCommitDuring: [
		self monticelloInstall: self metacelloFilenames.
		System commitTransaction ifFalse: [ ^self error: 'commit failed' ] ].
%
category: 'metacello installing'
method: GLASSBootstrapDriver
installPackageSpec: projectClassName repository: httpRepositoryDescription

	| repositorySpec |
	repositorySpec := self useRepositoryDirectory
		ifTrue: [
			self metacelloMCProjectClass new repositorySpec
				description: self repositoryDirectory;
				type: 'directory';
				yourself ]
		ifFalse: [
			self metacelloMCProjectClass new repositorySpec
				description: httpRepositoryDescription;
				type: 'http';
				yourself ].
	(self metacelloMCProjectClass new packageSpec
		name: projectClassName;
		repository: repositorySpec
		yourself) load.
	System commitTransaction ifFalse: [ ^self error: 'commit failed' ].
%
category: 'metacello installing'
method: GLASSBootstrapDriver
installProject: projectClassName version: versionString loads: loads

	| repositories version |
	version := (System myUserProfile symbolList objectNamed: projectClassName asSymbol) new project version: versionString.
	self useRepositoryDirectory
		ifTrue: [
			repositories := { self mCServerDirectoryRepositoryClass new 
								directory: (self serverFileDirectoryClass on: self repositoryDirectory). }.
			version repositoryOverrides: repositories ].

	loads isEmpty
		ifTrue: [ version load ]
		ifFalse: [ version load: loads ].
									
	System commitTransaction ifFalse: [ ^self error: 'commit failed' ].
%
category: 'private'
method: GLASSBootstrapDriver
log: anObject

	GsFile gciLogServer: '--transcript--', anObject printString
%
category: 'class accessing'
method: GLASSBootstrapDriver
mCCacheRepositoryClass

	^System myUserProfile symbolList objectNamed: #MCCacheRepository
%
category: 'class accessing'
method: GLASSBootstrapDriver
mCDictionaryRepositoryClass

	^System myUserProfile symbolList objectNamed: #MCDictionaryRepository
%
category: 'class accessing'
method: GLASSBootstrapDriver
mCHttpRepositoryClass

	^System myUserProfile symbolList objectNamed: #MCHttpRepository
%
category: 'class accessing'
method: GLASSBootstrapDriver
mCPlatformSupportClass

	^System myUserProfile symbolList objectNamed: #MCPlatformSupport
%
category: 'class accessing'
method: GLASSBootstrapDriver
mCServerDirectoryRepositoryClass

	^System myUserProfile symbolList objectNamed: #MCServerDirectoryRepository
%
category: 'mcz installing'
method: GLASSBootstrapDriver
mczInstall: filenames silently: silently

	| basePath |
	basePath := self repositoryDirectory.
	silently
		ifTrue: [
			self systemChangeNotifierClass uniqueInstance doSilently: [
				filenames do: [:filename |
					self log: 'MCZ load: ', filename.
					self mczInstallerClass
						installFileNamed: basePath, filename
						in: self bootStrapSymbolDictionary ]]]
		ifFalse: [
			filenames do: [:filename |
				self log: 'MCZ load: ', filename.
				self mczInstallerClass
					installFileNamed: basePath, filename
					in: self bootStrapSymbolDictionary ]].
	System commitTransaction ifFalse: [ ^self error: 'commit failed' ].
%
category: 'class accessing'
method: GLASSBootstrapDriver
mczInstallerClass

	^System myUserProfile symbolList objectNamed: #MczInstaller
%
category: 'accessing'
method: GLASSBootstrapDriver
metacelloFilenames

   ^metacelloFilenames
%
category: 'accessing'
method: GLASSBootstrapDriver
metacelloFilenames: newValue

    metacelloFilenames := newValue
%
category: 'class accessing'
method: GLASSBootstrapDriver
metacelloMCProjectClass

	^System myUserProfile symbolList objectNamed: #MetacelloMCProject
%
category: 'class accessing'
method: GLASSBootstrapDriver
metacelloMCSpecLoaderClass

	^System myUserProfile symbolList objectNamed: #MetacelloMCSpecLoader
%
category: 'accessing'
method: GLASSBootstrapDriver
metacelloVersionString

   ^metacelloVersionString
%
category: 'accessing'
method: GLASSBootstrapDriver
metacelloVersionString: newValue

   metacelloVersionString := newValue
%
category: 'accessing'
method: GLASSBootstrapDriver
monticelloFilenames

   ^monticelloFilenames
%
category: 'accessing'
method: GLASSBootstrapDriver
monticelloFilenames: newValue

    monticelloFilenames := newValue
%
category: 'metacello installing'
method: GLASSBootstrapDriver
monticelloInstall: filenames

	| repository |
	repository := self mCServerDirectoryRepositoryClass new 
						directory: (self serverFileDirectoryClass on: self repositoryDirectory).
	filenames do: [:filename | | version |
		self log: 'Monticello load: ', filename.
		version := repository loadVersionFromFileNamed: filename.
		version load ].
	System commitTransaction ifFalse: [ ^self error: 'commit failed' ].
%
category: 'testing'
method: GLASSBootstrapDriver
omniBrowserInstalled

	^(System myUserProfile symbolList objectNamed: #OBCommand) ~~ nil
%
category: 'mcz installing'
method: GLASSBootstrapDriver
postMczInstallInitialization

	| repository |
	repository := self mCDictionaryRepositoryClass new.
	self mCCacheRepositoryClass setDefault: repository.
	self mCPlatformSupportClass autoMigrate: false.
	
	System commitTransaction ifFalse: [ ^self error: 'commit failed' ].
%
category: 'accessing'
method: GLASSBootstrapDriver
repositoryDirectory

   ^repositoryDirectory
%
category: 'accessing'
method: GLASSBootstrapDriver
repositoryDirectory: newValue

   repositoryDirectory := newValue
%
category: 'accessing'
method: GLASSBootstrapDriver
repositoryMap

   ^repositoryMap
%
category: 'accessing'
method: GLASSBootstrapDriver
repositoryMap: newValue

   repositoryMap := newValue
%
category: 'class accessing'
method: GLASSBootstrapDriver
serverFileDirectoryClass

	^System myUserProfile symbolList objectNamed: #ServerFileDirectory
%
category: 'class accessing'
method: GLASSBootstrapDriver
systemChangeNotifierClass

	^System myUserProfile symbolList objectNamed: #SystemChangeNotifier
%
category: 'accessing'
method: GLASSBootstrapDriver
useRepositoryDirectory

   ^useRepositoryDirectory
%
category: 'accessing'
method: GLASSBootstrapDriver
useRepositoryDirectory: newValue

   useRepositoryDirectory := newValue
%

! Remove existing behavior from MczInstaller
removeallmethods MczInstaller
removeallclassmethods MczInstaller
! ------------------- Class methods for MczInstaller
category: 'versionInfo'
classmethod: MczInstaller
clearVersionInfo
	Versions _ Dictionary new
%
category: 'defaults'
classmethod: MczInstaller
defaultSymbolDictionary
    ^ GsSession currentSession objectNamed: #UserGlobals
%
category: 'services'
classmethod: MczInstaller
initialize
	self clearVersionInfo.
%
category: 'installing'
classmethod: MczInstaller
installFileNamed: aFileName
	self installFileNamed: aFileName in: self defaultSymbolDictionary
%
category: 'installing'
classmethod: MczInstaller
installFileNamed: aFileName in: aSymDict
	| file stream |
	file := GsFile open: aFileName mode: 'rb'.
	stream := RWBinaryOrTextStream on: file contents.
	stream name: aFileName.
	file close.
    self installStream: stream in: aSymDict
%
category: 'installing'
classmethod: MczInstaller
installStream: aStream
	self installStream: aStream in: self defaultSymbolDictionary
%
category: 'installing'
classmethod: MczInstaller
installStream: aStream in: aSymDict
    GsPackagePolicy current homeSymbolDict: aSymDict.
	(self on: aStream) install
%
category: 'services'
classmethod: MczInstaller
loadVersionFile: fileName
	self installFileNamed: fileName in: self defaultSymbolDictionary
%
category: 'services'
classmethod: MczInstaller
loadVersionFile: fileName in: aSymDict
	self installFileNamed: fileName in: aSymDict
%
category: 'instance creation'
classmethod: MczInstaller
on: aStream
	^ self new stream: aStream
%
category: 'versionInfo'
classmethod: MczInstaller
versionInfo
	^ Versions
%
! ------------------- Instance methods for MczInstaller
category: 'as yet unclassified'
method: MczInstaller
associate: tokens
	| result |
	result _ Dictionary new.
	tokens pairsDo: [:key :value | | v |
                    v := value.
					v isString ifFalse: [v _ v collect: [:ea | self associate: ea]].
					v = 'nil' ifTrue: [v _ ''].
					result at: key put: v].
	^ result
%
category: 'as yet unclassified'
method: MczInstaller
checkDependencies
	| dependencies unmet |
	dependencies _ (zip membersMatching: 'dependencies/*') 
			collect: [:member | self extractInfoFrom: (self parseMember: member)].
	unmet _ dependencies reject: [:dep |
		self versions: Versions anySatisfy: (dep at: #id)].
	^ unmet isEmpty or: [
		self error: (String streamContents: [:s|
			s nextPutAll: 'The following dependencies seem to be missing:'; cr.
			unmet do: [:each | s nextPutAll: (each at: #name); cr].
			s nextPutAll: 'Do you still want to install this package?'])]
%
category: 'as yet unclassified'
method: MczInstaller
extractInfoFrom: dict
	dict at: #id put: (UUID fromString: (dict at: #id)).
	dict at: #date ifPresent: [:dateString | 
		dict at: #date put: (
		[ | d |
			[d := Date fromString: dateString] on: Error do: [ :ex | ex return: nil ].
			d == nil ifTrue: [ d := Date fromString: dateString usingFormat: #(2 1 3 $/ 1 2 $: false ) ].
			d ] on: Error do: [ :ex | ex return: nil ])].
	dict at: #time ifPresent: [:t | t isEmpty ifFalse: [dict at: #time put: (Time readFrom: t readStream)]].
	dict at: #ancestors ifPresent: [:a | dict at: #ancestors put: (a collect: [:ea | self extractInfoFrom: ea])].
	^ dict
%
category: 'as yet unclassified'
method: MczInstaller
extractPackageName
	^ (self parseMember: 'package') at: #name.
	
%
category: 'as yet unclassified'
method: MczInstaller
extractVersionInfo
	^ self extractInfoFrom: (self parseMember: 'version')
%
category: 'as yet unclassified'
method: MczInstaller
install
	| sources |
	zip _ ZipArchive new.
	zip readFrom: stream.
	self checkDependencies ifFalse: [^false].
	"don't recordVersionInfo ... the extractVersionInfo method is recursive ... we don't depend upon the versionInfo recorded by
	 McsInstaller anyway .... once bootstrap with MczInstaller is done, we reload using Monticello"
	"self recordVersionInfo."
	sources _ (zip membersMatching: 'snapshot/*') 
				asSortedCollection: [:a :b | a fileName < b fileName].
	sources do: [:src | self installMember: src].
%
category: 'as yet unclassified'
method: MczInstaller
installMember: member
	 | str |
	self useNewChangeSetDuring:
		[str _ member contentStream text.
		str fileIn]
%
category: 'as yet unclassified'
method: MczInstaller
parseMember: fileName
	| tokens |
	tokens _ (self scanner scanTokens: (zip contentsOf: fileName)) first.
	^ self associate: tokens
%
category: 'as yet unclassified'
method: MczInstaller
recordVersionInfo
	Versions 
		at: self extractPackageName 
		put: self extractVersionInfo
%
category: 'as yet unclassified'
method: MczInstaller
scanner
	^ Scanner new
%
category: 'as yet unclassified'
method: MczInstaller
stream: aStream
	stream _ aStream
%
category: 'as yet unclassified'
method: MczInstaller
useNewChangeSetDuring: aBlock
	aBlock value
%
category: 'as yet unclassified'
method: MczInstaller
versions: aVersionList anySatisfy: aDependencyID
	^ aVersionList anySatisfy: [:version | 
			aDependencyID = (version at: #id)
				or: [self versions: (version at: #ancestors) anySatisfy: aDependencyID]]
%

! Remove existing behavior from Scanner
removeallmethods Scanner
removeallclassmethods Scanner
! ------------------- Class methods for Scanner
category: 'class initialization'
classmethod: Scanner
initialize
	| newTable |
	newTable _ Array new: 256 withAll: #xBinary. "default"
	newTable atAll: #(9 10 12 13 32 ) put: #xDelimiter. "tab lf ff cr space"
	newTable atAll: ($0 asciiValue to: $9 asciiValue) put: #xDigit.

	1 to: 255
		do: [:index |
			(Character value: index) isLetter
				ifTrue: [newTable at: index put: #xLetter]].

	newTable at: 30 put: #doIt.
	newTable at: $" asciiValue put: #xDoubleQuote.
	newTable at: $# asciiValue put: #xLitQuote.
	newTable at: $$ asciiValue put: #xDollar.
	newTable at: $' asciiValue put: #xSingleQuote.
	newTable at: $: asciiValue put: #xColon.
	newTable at: $( asciiValue put: #leftParenthesis.
	newTable at: $) asciiValue put: #rightParenthesis.
	newTable at: $. asciiValue put: #period.
	newTable at: $; asciiValue put: #semicolon.
	newTable at: $[ asciiValue put: #leftBracket.
	newTable at: $] asciiValue put: #rightBracket.
	newTable at: ${ asciiValue put: #leftBrace.
	newTable at: $} asciiValue put: #rightBrace.
	newTable at: $^ asciiValue put: #upArrow.
	newTable at: $_ asciiValue put: #leftArrow.
	newTable at: $| asciiValue put: #verticalBar.
	TypeTable _ newTable "bon voyage!"

	"Scanner initialize"
%
category: 'instance creation'
classmethod: Scanner
new

	^super new initScanner
%
! ------------------- Instance methods for Scanner
category: 'expression types'
method: Scanner
advance

	| prevToken |
	prevToken _ token.
	self scanToken.
	^prevToken
%
category: 'error handling'
method: Scanner
errorMultibyteCharacter

	self error: 'multi-byte character is found at unexpected place'.
%
category: 'initialize-release'
method: Scanner
initScanner

	buffer _ WriteStream on: (String new: 40).
	typeTable _ TypeTable
%
category: 'expression types'
method: Scanner
nextLiteral
	"Same as advance, but -4 comes back as a number instead of two tokens"

	| prevToken |
	prevToken _ self advance.
	(prevToken == #- and: [token isKindOf: Number])
		ifTrue: 
			[^self advance negated].
	^prevToken
%
category: 'error handling'
method: Scanner
notify: string 
	"Refer to the comment in Object|notify:." 
	self error: string
%
category: 'error handling'
method: Scanner
offEnd: aString 
	"Parser overrides this"

	^self notify: aString
%
category: 'expression types'
method: Scanner
revertToCheckpoint: checkpoint
	"Revert to the state when checkpoint was made."

	| myCopy |
	myCopy _ checkpoint first.
	1 to: self class instSize do:
		[:i | self instVarAt: i put: (myCopy instVarAt: i)].
	source _ checkpoint second.
	currentComment _ checkpoint third
%
category: 'initialize-release'
method: Scanner
scan: inputStream 
	"Bind the input stream, fill the character buffers and first token buffer."

	source _ inputStream.
	self step.
	self step.
	self scanToken
%
category: 'public access'
method: Scanner
scanFieldNames: stringOrArray
	"Answer an Array of Strings that are the identifiers in the input string, 
	stringOrArray. If passed an Array, just answer with that Array, i.e., 
	assume it has already been scanned."

	| strm |
	(stringOrArray isMemberOf: Array)
		ifTrue: [^stringOrArray].
	self scan: (ReadStream on: stringOrArray asString).
	strm _ WriteStream on: (Array new: 10).
	[tokenType = #doIt]
		whileFalse: 
			[tokenType = #word ifTrue: [strm nextPut: token].
			self scanToken].
	^strm contents

	"Scanner new scanFieldNames: 'abc  def ghi' ('abc' 'def' 'ghi' )"
%
category: 'expression types'
method: Scanner
scanLitVec

	| s |
	s _ WriteStream on: (Array new: 16).
	[tokenType = #rightParenthesis or: [tokenType = #doIt]]
		whileFalse: 
			[tokenType = #leftParenthesis
				ifTrue: 
					[self scanToken; scanLitVec]
				ifFalse: 
					[tokenType = #word | (tokenType = #keyword) | (tokenType = #colon)
						ifTrue: 
							[self scanLitWord.
							token = #true ifTrue: [token _ true].
							token = #false ifTrue: [token _ false].
							token = #nil ifTrue: [token _ nil]]
						ifFalse:
							[(token == #- 
									and: [((typeTable at: hereChar charCode ifAbsent: [#xLetter])) = #xDigit])
								ifTrue: 
									[self scanToken.
									token _ token negated]]].
			s nextPut: token.
			self scanToken].
	token _ s contents
%
category: 'expression types'
method: Scanner
scanLitWord
	"Accumulate keywords and asSymbol the result."

	| t |
	[(typeTable at: hereChar asciiValue ifAbsent: [#xLetter]) = #xLetter] whileTrue: [
		t _ token.
		self xLetter.
		token _ t , token
	].
	token _ token asSymbol.
%
category: 'public access'
method: Scanner
scanMessageParts: sourceString
	"Return an array of the form (comment keyword comment arg comment keyword comment arg comment) for the message pattern of this method.  Courtesy of Ted Kaehler, June 1999"

	| coll nonKeywords |
	coll _ OrderedCollection new.
	self scan: (ReadStream on: sourceString asString).
	nonKeywords _ 0.
	[tokenType = #doIt] whileFalse:
		[(currentComment == nil or: [currentComment isEmpty])
			ifTrue: [coll addLast: nil]
			ifFalse: [coll addLast: currentComment removeFirst.
				[currentComment isEmpty] whileFalse:
					[coll at: coll size put: (coll last, ' ', currentComment removeFirst)]].
		(token numArgs < 1 or: [(token = #|) and: [ coll size > 1 ]])
			ifTrue: [(nonKeywords _ nonKeywords + 1) > 1 ifTrue: [^ coll]]
						"done with header"
			ifFalse: [nonKeywords _ 0].
		coll addLast: token.
		self scanToken].
	(currentComment == nil or: [currentComment isEmpty])
		ifTrue: [coll addLast: nil]
		ifFalse: [coll addLast: currentComment removeFirst.
			[currentComment isEmpty] whileFalse: [
				coll at: coll size put: (coll last, ' ', currentComment removeFirst)]].
	^ coll
%
category: 'expression types'
method: Scanner
scanStringStruct

	| s |
	s _ WriteStream on: (Array new: 16).
	[tokenType = #rightParenthesis or: [tokenType = #doIt]]
		whileFalse: 
			[tokenType = #leftParenthesis
				ifTrue: 
					[self scanToken; scanStringStruct]
				ifFalse: 
					[tokenType = #word ifFalse:
						[^self error: 'only words and parens allowed']].
			s nextPut: token.
			self scanToken].
	token _ s contents
%
category: 'public access'
method: Scanner
scanStringStruct: textOrString 
	"The input is a string whose elements are identifiers and parenthesized
	 groups of identifiers.  Answer an array reflecting that structure, representing
	 each identifier by an uninterned string."

	self scan: (ReadStream on: textOrString asString).
	self scanStringStruct.
	^token

	"Scanner new scanStringStruct: 'a b (c d) (e f g)'"
%
category: 'expression types'
method: Scanner
scanToken

	[(tokenType _ typeTable at: hereChar asciiValue ifAbsent: [#xLetter]) == #xDelimiter]
		whileTrue: [self step].  "Skip delimiters fast, there almost always is one."
	mark _ source position - 1.
	(tokenType at: 1) = $x "x as first letter"
		ifTrue: [self perform: tokenType "means perform to compute token & type"]
		ifFalse: [token _ self step asSymbol "else just unique the first char"].
	^ token.
%
category: 'public access'
method: Scanner
scanTokens: textOrString 
	"Answer an Array that has been tokenized as though the input text, 
	textOrString, had appeared between the array delimitors #( and ) in a 
	Smalltalk literal expression."

	self scan: (ReadStream on: textOrString asString).
	self scanLitVec.
	^token

	"Scanner new scanTokens: 'identifier keyword: 8r31 ''string'' .'"
%
category: 'expression types'
method: Scanner
step

	| c |
	c _ hereChar.
	hereChar _ aheadChar.
	source atEnd
		ifTrue: [aheadChar _ 30 asCharacter "doit"]
		ifFalse: [aheadChar _ source next].
	^c
%
category: 'multi-character scans'
method: Scanner
xBinary

	tokenType _ #binary.
	token _ self step asSymbol.
	[| type | 
	type _ typeTable at: hereChar asciiValue ifAbsent: [#xLetter].
	type == #xBinary and: [hereChar ~= $-]] whileTrue: [
		token _ (token, (String with: self step)) asSymbol].
%
category: 'multi-character scans'
method: Scanner
xColon		"Allow := for assignment by converting to #:= "
	aheadChar = $= ifTrue:
		[self step.
		tokenType _ #leftArrow.
		self step.
		^ token _ #'_'].
	"Otherwise, just do what normal scan of colon would do"
	tokenType _ #colon.
	^ token _ self step asSymbol
%
category: 'multi-character scans'
method: Scanner
xDelimiter
	"Ignore blanks, etc."

	self scanToken
%
category: 'multi-character scans'
method: Scanner
xDigit
	"Form a number."

	tokenType _ #number.
	(aheadChar = 30 asCharacter and: [source atEnd
			and:  [source skip: -1. source next ~= 30 asCharacter]])
		ifTrue: [source skip: -1 "Read off the end last time"]
		ifFalse: [source skip: -2].
	token _ [Number readFrom: source] ifError: [:err :rcvr | self offEnd: err].
	self step; step
%
category: 'multi-character scans'
method: Scanner
xDollar
	"Form a Character literal."

	self step. "pass over $"
	token _ self step.
	tokenType _ #number "really should be Char, but rest of compiler doesn't know"
%
category: 'multi-character scans'
method: Scanner
xDoubleQuote

    "Collect a comment."
    "wod 1/10/98: Allow 'empty' comments by testing the first character
for $"" rather than blindly adding it to the comment being collected."
    | aStream stopChar |
    stopChar _ 30 asCharacter.
    aStream _ WriteStream on: (String new: 200).
    self step.
    [hereChar = $"]
        whileFalse:
            [(hereChar = stopChar and: [source atEnd])
                ifTrue: [^self offEnd: 'Unmatched comment quote'].
            aStream nextPut: self step.].
    self step.
    currentComment == nil
        ifTrue: [currentComment _ OrderedCollection with: aStream
contents]
        ifFalse: [currentComment add: aStream contents].
    self scanToken.
%
category: 'multi-character scans'
method: Scanner
xLetter
	"Form a word or keyword."

	| type c |
	buffer reset.
	[c _ hereChar asciiValue.
	(type _ typeTable at: c ifAbsent: [#xLetter]) == #xLetter or: [type == #xDigit]]
		whileTrue: ["open code step for speed"
			buffer nextPut: hereChar.
			hereChar _ aheadChar.
			source atEnd
				ifTrue: [aheadChar _ 30 asCharacter
					"doit"]
				ifFalse: [aheadChar _ source next]].
	(type == #colon or: [type == #xColon and: [aheadChar ~= $=]])
		ifTrue: [buffer nextPut: self step.
			["Allow any number of embedded colons in literal symbols"
			(typeTable at: hereChar asciiValue ifAbsent: [#xLetter])
				== #xColon]
				whileTrue: [buffer nextPut: self step].
			tokenType _ #keyword]
		ifFalse: [tokenType _ #word].
	token _ buffer contents.
	token isOctetString ifTrue: [token _ token asOctetString].
%
category: 'multi-character scans'
method: Scanner
xLitQuote
	"Symbols and vectors: #(1 (4 5) 2 3) #ifTrue:ifFalse: #'abc'."

	| start |
	start _ mark.
	self step. "litQuote"
	self scanToken.
	tokenType = #leftParenthesis
		ifTrue: 
			[self scanToken; scanLitVec.
			mark _ start+1.
			tokenType == #doIt
				ifTrue: [self offEnd: 'Unmatched parenthesis']]
		ifFalse: 
			[(#(word keyword colon ) includes: tokenType) 
				ifTrue:
					[self scanLitWord]
				ifFalse:
					[(tokenType==#literal)
						ifTrue:
							[(token isSymbol)
								ifTrue: "##word"
									[token _ token "May want to move toward ANSI here"]]
						ifFalse:
							[tokenType==#string ifTrue: [token _ token asSymbol]]]].
	mark _ start.
	tokenType _ #literal

"	#(Pen)
	#Pen
	#'Pen'
	##Pen
	###Pen
"
%
category: 'multi-character scans'
method: Scanner
xSingleQuote
	"String."

	self step.
	buffer reset.
	[hereChar = $' and: [aheadChar = $' ifTrue: [self step. false] ifFalse: [true]]] whileFalse: [
		buffer nextPut: self step.
		(hereChar = 30 asCharacter and: [source atEnd])
			ifTrue: [^self offEnd: 'Unmatched string quote']].
	self step.
	token _ buffer contents.
	token isOctetString ifTrue: [token _ token asOctetString].
	tokenType _ #string.
%

! Remove existing behavior from ZipReadStream
removeallmethods ZipReadStream
removeallclassmethods ZipReadStream
! ------------------- Class methods for ZipReadStream
category: 'class initialization'
classmethod: ZipReadStream
initialize
	"ZipReadStream initialize"
	| low high |

	MaxBits := 16.
	StateNewBlock := 0.
	StateNoMoreData := 1.
	BlockProceedBit := 8.
	BlockTypes := #(	processStoredBlock	"New block in stored format"
					processFixedBlock	"New block with fixed huffman tables"
					processDynamicBlock	"New block with dynamic huffman tables"
					errorBadBlock		"Bad block format"
					proceedStoredBlock	"Continue block in stored format"
					proceedFixedBlock	"Continue block in fixed format"
					proceedDynamicBlock	"Continue block in dynamic format"
					errorBadBlock		"Bad block format").
	"Initialize fixed block values"
	FixedLitCodes := 	((1 to: 144) collect:[:i| 8]),
					((145 to: 256) collect:[:i| 9]),
					((257 to: 280) collect:[:i| 7]),
					((281 to: 288) collect:[:i| 8]).
	FixedDistCodes := ((1 to: 32) collect:[:i| 5]).

	"Init literal/length map"
	low := #(3 4 5 6 7 8 9 10 11 13 15 17 19 23 27 31 35 43 51 59 67 83 99 115 131 163 195 227 258 ).
	high := #(0 0 0 0 0 0 0 0 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 5 5 5 5 0 0).
	LiteralLengthMap := WordArray new: 256 + 32.
	1 to: 257 do:[:i| LiteralLengthMap at: i put: i-1].
	1 to: 29 do:[:i| LiteralLengthMap at: 257+i put: (low at:i) + ( (high at: i) + 1 << 16)].

	"Init distance map"
	high := #(0 0 0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10 11 11 12 12 13 13).
	low := #(1 2 3 4 5 7 9 13 17 25 33 49 65 97 129 193 257 385 513 769
			1025 1537 2049 3073 4097 6145 8193 12289 16385 24577).
	DistanceMap := WordArray new: 32.
	1 to: 30 do:[:i| DistanceMap at: i put: (low at: i) + ( (high at: i) << 16)].

	"Init fixed block huffman tables"
	FixedLitTable := self basicNew
				huffmanTableFrom: FixedLitCodes
				mappedBy: LiteralLengthMap.
	FixedDistTable := self basicNew
				huffmanTableFrom: FixedDistCodes
				mappedBy: DistanceMap.
%
! ------------------- Instance methods for ZipReadStream
category: 'testing'
method: ZipReadStream
atEnd
	"Note: It is possible that we have a few bits left,
	representing just the EOB marker. To check for
	this we must force decompression of the next
	block if at end of data."
	position < readLimit ifTrue:[^false]. "Primitive test"
	(position >= readLimit and:[state = StateNoMoreData]) ifTrue:[^true].
	"Force decompression, by calling #next. Since #moveContentsToFront
	will never move data to the beginning of the buffer it is safe to
	skip back the read position afterwards"
	self next == nil ifTrue:[^true].
	position := position - 1.
	^false
%
category: 'private'
method: ZipReadStream
bitPosition
	"Return the current bit position of the source"
	sourceStream == nil
		ifTrue:[^sourcePos * 8 + bitPos]
		ifFalse:[^sourceStream position - 1 + sourcePos * 8 + bitPos]
%
category: 'huffman trees'
method: ZipReadStream
computeHuffmanValues: aCollection counts: counts from: minBits to: maxBits
	"Assign numerical values to all codes.
	Note: The values are stored according to the bit length"
	| offsets values baseOffset codeLength |
	offsets := Array new: maxBits.
	offsets atAllPut: 0.
	baseOffset := 1.
	minBits to: maxBits do:[:bits|
		offsets at: bits put: baseOffset.
		baseOffset := baseOffset + (counts at: bits+1)].
	values := WordArray new: aCollection size.
	1 to: aCollection size do:[:i|
		codeLength := aCollection at: i.
		codeLength > 0 ifTrue:[
			baseOffset := offsets at: codeLength.
			values at: baseOffset put: i-1.
			offsets at: codeLength put: baseOffset + 1]].
	^values
%
category: 'huffman trees'
method: ZipReadStream
createHuffmanTables: values counts: counts from: minBits to: maxBits
	"Create the actual tables"
	| table tableStart tableSize tableEnd 
	valueIndex tableStack numValues deltaBits maxEntries
	lastTable lastTableStart tableIndex lastTableIndex |

	table := WordArray new: ((4 bitShift: minBits) max: 16).

	"Create the first entry - this is a dummy.
	It gives us information about how many bits to fetch initially."
	table at: 1 put: (minBits bitShift: 24) + 2. "First actual table starts at index 2"

	"Create the first table from scratch."
	tableStart := 2. "See above"
	tableSize := 1 bitShift: minBits.
	tableEnd := tableStart + tableSize.
	"Store the terminal symbols"
	valueIndex := (counts at: minBits+1).
	tableIndex := 0.
	1 to: valueIndex do:[:i|
		table at: tableStart + tableIndex put: (values at: i).
		tableIndex := self increment: tableIndex bits: minBits].
	"Fill up remaining entries with invalid entries"
	tableStack := OrderedCollection new: 10. "Should be more than enough"
	tableStack addLast: 
		(Array 
			with: minBits	"Number of bits (e.g., depth) for this table"
			with: tableStart	"Start of table"
			with: tableIndex "Next index in table"
			with: minBits	"Number of delta bits encoded in table"
			with: tableSize - valueIndex "Entries remaining in table").
	"Go to next value index"
	valueIndex := valueIndex + 1.
	"Walk over remaining bit lengths and create new subtables"
	minBits+1 to: maxBits do:[:bits|
		numValues := counts at: bits+1.
		[numValues > 0] whileTrue:["Create a new subtable"
			lastTable := tableStack last.
			lastTableStart := lastTable at: 2.
			lastTableIndex := lastTable at: 3.
			deltaBits := bits - (lastTable at: 1).
			"Make up a table of deltaBits size"
			tableSize := 1 bitShift: deltaBits.
			tableStart := tableEnd.
			tableEnd := tableEnd + tableSize.
			[tableEnd > table size ]
				whileTrue:[table := self growHuffmanTable: table].
			"Connect to last table"
			self assert:[(table at: lastTableStart + lastTableIndex) = 0]."Entry must be unused"
			table at: lastTableStart + lastTableIndex put: (deltaBits bitShift: 24) + tableStart.
			lastTable at: 3 put: (self increment: lastTableIndex bits: (lastTable at: 4)).
			lastTable at: 5 put: (lastTable at: 5) - 1.
			self assert:[(lastTable at: 5) >= 0]. "Don't exceed tableSize"
			"Store terminal values"
			maxEntries := numValues min: tableSize.
			tableIndex := 0.
			1 to: maxEntries do:[:i|
				table at: tableStart + tableIndex put: (values at: valueIndex).
				valueIndex := valueIndex + 1.
				numValues := numValues - 1.
				tableIndex := self increment: tableIndex bits: deltaBits].
			"Check if we have filled up the current table completely"
			maxEntries = tableSize ifTrue:[
				"Table has been filled. Back up to the last table with space left."
				[tableStack isEmpty not and:[(tableStack last at: 5) = 0]]
						whileTrue:[tableStack removeLast].
			] ifFalse:[
				"Table not yet filled. Put it back on the stack."
				tableStack addLast:
					(Array
						with: bits		"Nr. of bits in this table"
						with: tableStart	"Start of table"
						with: tableIndex "Index in table"
						with: deltaBits	"delta bits of table"
						with: tableSize - maxEntries "Unused entries in table").
			].
		].
	].
	 ^table copyFrom: 1 to: tableEnd-1
%
category: 'huffman trees'
method: ZipReadStream
decodeDynamicTable: nItems from: aHuffmanTable
	"Decode the code length of the literal/length and distance table
	in a block compressed with dynamic huffman trees"
	| values index value repCount theValue |
	values := Array new: nItems.
	index := 1.
	theValue := 0.
	[index <= nItems] whileTrue:[
		value := self decodeValueFrom: aHuffmanTable.
		value < 16 ifTrue:[
			"Immediate values"
			theValue := value.
			values at: index put: value.
			index := index+1.
		] ifFalse:[
			"Repeated values"
			value = 16 ifTrue:[
				"Repeat last value"
				repCount := (self nextBits: 2) + 3.
			] ifFalse:[
				"Repeat zero value"
				theValue := 0.
				value = 17 
					ifTrue:[repCount := (self nextBits: 3) + 3]
					ifFalse:[value = 18 
								ifTrue:[repCount := (self nextBits: 7) + 11]
								ifFalse:[^self error:'Invalid bits tree value']]].
			0 to: repCount-1 do:[:i| values at: index+i put: theValue].
			index := index + repCount].
	].
	^values
%
category: 'inflating'
method: ZipReadStream
decodeValueFrom: table
	"Decode the next value in the receiver using the given huffman table."
	| bits bitsNeeded tableIndex value |
	bitsNeeded := (table at: 1) bitShift: -24.	"Initial bits needed"
	tableIndex := 2.							"First real table"
	[bits := self nextSingleBits: bitsNeeded.	"Get bits"
	value := table at: (tableIndex + bits).		"Lookup entry in table"
	(value bitAnd: 16r3F000000) = 0] 			"Check if it is a non-leaf node"
		whileFalse:["Fetch sub table"
			tableIndex := value bitAnd: 16rFFFF.	"Table offset in low 16 bit"
			bitsNeeded := (value bitShift: -24) bitAnd: 255. "Additional bits in high 8 bit"
			bitsNeeded > MaxBits ifTrue:[^self error:'Invalid huffman table entry']].
	^value
%
category: 'inflating'
method: ZipReadStream
decompressBlock: llTable with: dTable
	"Process the compressed data in the block.
	llTable is the huffman table for literal/length codes
	and dTable is the huffman table for distance codes."
	| value extra length distance oldPos oldBits oldBitPos |
	[readLimit < itsCollection size and:[sourcePos <= sourceLimit]] whileTrue:[
		"Back up stuff if we're running out of space"
		oldBits := bitBuf.
		oldBitPos := bitPos.
		oldPos := sourcePos.
		value := self decodeValueFrom: llTable.
		value < 256 ifTrue:[ "A literal"
			itsCollection at: (readLimit := readLimit + 1) put: value.
		] ifFalse:["length/distance or end of block"
			value = 256 ifTrue:["End of block"
				state := state bitAnd: StateNoMoreData.
				^self].
			"Compute the actual length value (including possible extra bits)"
			extra := (value bitShift: -16) - 1.
			length := value bitAnd: 16rFFFF.
			extra > 0 ifTrue:[length := length + (self nextBits: extra)].
			"Compute the distance value"
			value := self decodeValueFrom: dTable.
			extra := (value bitShift: -16).
			distance := value bitAnd: 16rFFFF.
			extra > 0 ifTrue:[distance := distance + (self nextBits: extra)].
			(readLimit + length >= itsCollection size) ifTrue:[
				bitBuf := oldBits.
				bitPos := oldBitPos.
				sourcePos := oldPos.
				^self].
			itsCollection 
					squeakReplaceFrom: readLimit+1 
					to: readLimit + length + 1 
					with: itsCollection 
					startingAt: readLimit - distance + 1.
			readLimit := readLimit + length.
		].
	].
%
category: 'huffman trees'
method: ZipReadStream
distanceMap
	^DistanceMap
%
category: 'crc'
method: ZipReadStream
expectedCrc: aNumberOrNil
	"If expectedCrc is set, it will be compared against the calculated CRC32 in verifyCrc.
	This number should be the number read from the Zip header (which is the bitwise complement of my crc if all is working correctly)"
	expectedCrc := aNumberOrNil
%
category: 'private'
method: ZipReadStream
getFirstBuffer
	"Get the first source buffer after initialization has been done"
	sourceStream == nil ifTrue:[^self].
	source := sourceStream next: 1 << 16. "This is more than enough..."
	sourceLimit := source size.
%
category: 'private'
method: ZipReadStream
getNextBlock
	^self nextBits: 3
%
category: 'huffman trees'
method: ZipReadStream
growHuffmanTable: table
	| newTable |
	newTable := table species new: table size * 2.
	newTable replaceFrom: 1 to: table size with: table startingAt: 1.
	^newTable
%
category: 'huffman trees'
method: ZipReadStream
huffmanTableFrom: aCollection mappedBy: valueMap
	"Create a new huffman table from the given code lengths.
	Map the actual values by valueMap if it is given.
	See the class comment for a documentation of the huffman
	tables used in this decompressor."
	| counts  values table minBits maxBits |
	minBits := MaxBits + 1.
	maxBits := 0.
	"Count the occurences of each code length and compute minBits and maxBits"
	counts := Array new: MaxBits+1.
	counts atAllPut: 0.
	aCollection do:[:length| 
		length > 0 ifTrue:[
			length < minBits ifTrue:[minBits := length].
			length > maxBits ifTrue:[maxBits := length].
			counts at: length+1 put: (counts at: length+1)+1]].
	maxBits = 0 ifTrue:[^nil]. "Empty huffman table"

	"Assign numerical values to all codes."
	values := self computeHuffmanValues: aCollection counts: counts from: minBits to: maxBits.

	"Map the values if requested"
	self mapValues: values by: valueMap.

	"Create the actual tables"
	table := self createHuffmanTables: values counts: counts from: minBits to: maxBits.

	^table
%
category: 'huffman trees'
method: ZipReadStream
increment: value bits: nBits
	"Increment value in reverse bit order, e.g. 
	for a 3 bit value count as follows:
		000 / 100 / 010 / 110
		001 / 101 / 011 / 111
	See the class comment why we need this."
	| result bit |
	result := value.
	"Test the lowest bit first"
	bit := 1 << (nBits - 1).
	"If the currently tested bit is set then we need to
	turn this bit off and test the next bit right to it"
	[(result bitAnd: bit) = 0] whileFalse:[ 
		"Turn off current bit"
		result := result bitXor: bit.
		"And continue testing the next bit"
		bit := bit bitShift: -1].
	"Turn on the right-most bit that we haven't touched in the loop above"
	^result bitXor: bit
%
category: 'huffman trees'
method: ZipReadStream
literalLengthMap
	^LiteralLengthMap
%
category: 'huffman trees'
method: ZipReadStream
mapValues: values by: valueMap
	| oldValue |
	valueMap ifNil:[^values].
	1 to: values size do:[:i|
		oldValue := values at: i.
		"Note: there may be nil values if not all values are used"
		oldValue isNil
			ifTrue:[^values]
			ifFalse:[values at: i put: (valueMap at: oldValue+1)]].
%
category: 'private'
method: ZipReadStream
moveContentsToFront
	"Move the decoded contents of the receiver to the front so that we have enough space for decoding more data."
	| delta |
	readLimit > 32768 ifTrue:[
		delta := readLimit - 32767.
		itsCollection 
			replaceFrom: 1 
			to: itsCollection size - delta + 1 
			with: itsCollection 
			startingAt: delta.
		position := position - delta + 1.
		readLimit := readLimit - delta + 1].
%
category: 'private'
method: ZipReadStream
moveSourceToFront
	"Move the encoded contents of the receiver to the front so that we have enough space for decoding more data."
	(sourceStream == nil or:[sourceStream atEnd]) ifTrue:[^self].
	sourcePos > 10000 ifTrue:[
		source 
			replaceFrom: 1 
			to: source size - sourcePos
			with: source 
			startingAt: sourcePos + 1.
		source := sourceStream 
			next: sourcePos 
			into: source 
			startingAt: source size - sourcePos + 1.
		sourcePos := 0.
		sourceLimit := source size].
%
category: 'accessing'
method: ZipReadStream
next

(position > readLimit)
   ifTrue: [ ^ self pastEndRead ].
position := position + 1.
^ itsCollection at: (position - 1)
%
category: 'accessing'
method: ZipReadStream
next: n into: buffer startingAt: startIndex
	"Read n objects into the given collection. 
	Return aCollection or a partial copy if less than
	n elements have been read."
	| c numRead count |
	numRead := 0.
	["Force decompression if necessary"
	self atEnd ifTrue: [ ^buffer copyFrom: 1 to: startIndex+numRead-1].
	c := self next.
	"Store the first value which provoked decompression"
	buffer at: startIndex + numRead put: c.
	numRead := numRead + 1.
	"After collection has been filled copy as many objects as possible"
	count := (readLimit - position + 1) min: (n - numRead).
	buffer 
		replaceFrom: startIndex + numRead 
		to: startIndex + numRead + count - 1 
		with: itsCollection 
		startingAt: position.
	position := position + count.
	numRead := numRead + count.
	numRead = n] whileFalse.
	^buffer
%
category: 'bit access'
method: ZipReadStream
nextBits: n
	| bits |
	[bitPos < n] whileTrue:[
		bitBuf := bitBuf + (self nextByte bitShift: bitPos).
		bitPos := bitPos + 8].
	bits := bitBuf bitAnd: (1 bitShift: n)-1.
	bitBuf := bitBuf bitShift: 0 - n.
	bitPos := bitPos - n.
	^bits
%
category: 'bit access'
method: ZipReadStream
nextByte
	^source byteAt: (sourcePos := sourcePos + 1)
%
category: 'bit access'
method: ZipReadStream
nextSingleBits: n
	"Fetch the bits all at once"
	^self nextBits: n.
%
category: 'initialize'
method: ZipReadStream
on: aCollection from: firstIndex to: lastIndex
	bitBuf := bitPos := 0.
	"The decompression buffer has a size of at 64k,
	since we may have distances up to 32k back and
	repetitions of at most 32k length forward"
	itsCollection := aCollection species new: 1 << 16.
	readLimit := 0. "Not yet initialized"
	position := 1.
	source := aCollection.
	sourceLimit := lastIndex.
	sourcePos := firstIndex - 1.
	state := StateNewBlock.

	crc := 16rFFFFFFFF.
	expectedCrc := nil.
%
category: 'private'
method: ZipReadStream
pastEndRead
	"A client has attempted to read beyond the read limit.
	Check in what state we currently are and perform
	the appropriate action"
	| blockType bp oldLimit |
	state = StateNoMoreData ifTrue:[^nil]. "Get out early if possible"
	"Check if we can move decoded data to front"
	self moveContentsToFront.
	"Check if we can fetch more source data"
	self moveSourceToFront.
	state = StateNewBlock ifTrue:[state := self getNextBlock].
	blockType := state bitShift: -1.
	bp := self bitPosition.
	oldLimit := readLimit.
	self perform: (BlockTypes at: blockType+1).
	"Note: if bit position hasn't advanced then nothing has been decoded."
	bp = self bitPosition 
		ifTrue:[^self _primitiveFailed: #pastEndRead].
	"Update crc for the decoded contents"
	readLimit > oldLimit 
		ifTrue:[crc := self updateCrc: crc from: oldLimit+1 to: readLimit in: itsCollection].
	state = StateNoMoreData ifTrue:[self verifyCrc].
	^self next
%
category: 'inflating'
method: ZipReadStream
proceedDynamicBlock
	self decompressBlock: litTable with: distTable
%
category: 'inflating'
method: ZipReadStream
proceedFixedBlock
	self decompressBlock: litTable with: distTable
%
category: 'inflating'
method: ZipReadStream
proceedStoredBlock
	"Proceed decompressing a stored (e.g., uncompressed) block"
	| length decoded |
	"Literal table must be nil for a stored block"
	litTable == nil ifFalse:[^self error:'Bad state'].
	length := distTable.
	[length > 0 and:[readLimit < itsCollection size and:[sourcePos < sourceLimit]]] 
		whileTrue:[
			itsCollection at: (readLimit := readLimit + 1) put: 
				(source at: (sourcePos := sourcePos + 1)).
			length _ length - 1].
	length = 0 ifTrue:[state := state bitAnd: StateNoMoreData].
	decoded := length - distTable.
	distTable := length.
	^decoded
%
category: 'inflating'
method: ZipReadStream
processDynamicBlock
	| nLit nDist nLen codeLength lengthTable bits |
	nLit := (self nextBits: 5) + 257.
	nDist := (self nextBits: 5) + 1.
	nLen := (self nextBits: 4) + 4.
	codeLength := Array new: 19.
	codeLength atAllPut: 0.
	1 to: nLen do:[:i|
		bits := #(16 17 18 0 8 7 9 6 10 5 11 4 12 3 13 2 14 1 15) at: i.
		codeLength at: bits+1 put: (self nextBits: 3).
	].
	lengthTable := self huffmanTableFrom: codeLength mappedBy: nil.
	"RFC 1951: In other words, all code lengths form a single sequence..."
	codeLength := self decodeDynamicTable: nLit+nDist from: lengthTable.
	litTable := self 
				huffmanTableFrom: (codeLength copyFrom: 1 to: nLit)
				mappedBy: self literalLengthMap.
	distTable := self 
				huffmanTableFrom: (codeLength copyFrom: nLit+1 to: codeLength size)
				mappedBy: self distanceMap.
	state := state bitOr: BlockProceedBit.
	self proceedDynamicBlock.
%
category: 'inflating'
method: ZipReadStream
processFixedBlock
	litTable := FixedLitTable.
	distTable := FixedDistTable.
	state := state bitOr: BlockProceedBit.
	self proceedFixedBlock.
%
category: 'inflating'
method: ZipReadStream
processStoredBlock
	| chkSum length |
	"Skip to byte boundary"
	self nextBits: (bitPos bitAnd: 7).
	length := self nextBits: 16.
	chkSum := self nextBits: 16.
	(chkSum bitXor: 16rFFFF) = length
		ifFalse:[^self error:'Bad block length'].
	litTable := nil.
	distTable := length.
	state := state bitOr: BlockProceedBit.
	^self proceedStoredBlock
%
category: 'crc'
method: ZipReadStream
updateCrc: oldCrc from: start to: stop in: aCollection
	^ZipWriteStream updateCrc: oldCrc from: start to: stop in: aCollection
%
category: 'accessing'
method: ZipReadStream
upToEnd
	"Answer a subcollection from the current access position through the last element of the receiver."

	| newStream buffer |
	buffer := itsCollection species new: 100.
	newStream := WriteStream on: (itsCollection species new).
	[self atEnd] whileFalse: [newStream nextPutAll: (self next: buffer size into: buffer startingAt: 1)].
	^ newStream contents
%
category: 'crc'
method: ZipReadStream
verifyCrc
	"Verify the CRC-32 checksum calculated from the input against the expected CRC-32, if any.
	Answer the calculated CRC-32 in any case.
	Note that the CRC-32 used in Zip files is actually the bit inverse of the calculated value, so that is what is returned."

	| invertedCrc |
	invertedCrc := crc bitXor: 16rFFFFFFFF.
	(expectedCrc notNil and: [ expectedCrc ~= invertedCrc ])
		ifTrue: [ ^ self error: ('Wrong CRC-32 (expected ', expectedCrc hex printString, ' got ', invertedCrc hex printString, ') (proceed to ignore)' ) ].
	^invertedCrc
%
category: 'initialize'
method: ZipReadStream
_initStreamWith: aCollectionOrStream

	aCollectionOrStream isStream 
		ifTrue:[	aCollectionOrStream binary.
				sourceStream := aCollectionOrStream.
				self getFirstBuffer]
		ifFalse:[source := aCollectionOrStream].
	^self on: source from: 1 to: source size.
%

! Remove existing behavior from ReadWriteStream
removeallmethods ReadWriteStream
removeallclassmethods ReadWriteStream
! ------------------- Class methods for ReadWriteStream
category: 'Instance Creation'
classmethod: ReadWriteStream
on: aCollection from: firstIndex to: lastIndex 
	"Answer a new instance of the receiver, streaming over a copy of aCollection from
	firstIndex to lastIndex."

	^self on: (aCollection copyFrom: firstIndex to: lastIndex)
%
! ------------------- Instance methods for ReadWriteStream
category: 'Accessing'
method: ReadWriteStream
contents

^itsCollection
%
category: 'Accessing'
method: ReadWriteStream
next

"Returns the next object that the receiver can access for reading.  Generates
 an error if an attempt is made to read beyond the end of the stream."

(self atEnd)
   ifTrue: [ ^ self _error: #rtErrEofOnReadStream].
position := position + 1.
^ itsCollection at: (position - 1)
%

! Remove existing behavior from RWBinaryOrTextStream
removeallmethods RWBinaryOrTextStream
removeallclassmethods RWBinaryOrTextStream
! ------------------- Class methods for RWBinaryOrTextStream
category: 'Instance Creation'
classmethod: RWBinaryOrTextStream
on: aCollection

	| new |
	new := super on: aCollection asString.
	aCollection class = ByteArray
		ifTrue: [ new binary ].
	^new
%
! ------------------- Instance methods for RWBinaryOrTextStream
category: 'accessing'
method: RWBinaryOrTextStream
ascii
	isBinary := false
%
category: 'accessing'
method: RWBinaryOrTextStream
binary

	isBinary := true
%
category: 'accessing'
method: RWBinaryOrTextStream
contents

	^self isBinary
		ifTrue: [ itsCollection asByteArray ]
		ifFalse: [ itsCollection asString ]
%
category: 'fileIn/Out'
method: RWBinaryOrTextStream
fileIn

	| val chunk |
	[self atEnd] whileFalse:  [
		self skipSeparators.
		val := (self peekFor: $!) 
			ifTrue: [ | chunk classReader |
				chunk := self nextChunk.
				classReader := chunk evaluate.
				classReader scanFrom: self]
			ifFalse: 
				[self nextChunk evaluate]	
	].
	^val
%
category: 'accessing'
method: RWBinaryOrTextStream
isBinary

	^isBinary
%
category: 'accessing'
method: RWBinaryOrTextStream
name
	
	^name
%
category: 'accessing'
method: RWBinaryOrTextStream
name: aString

	name := aString
%
category: 'accessing'
method: RWBinaryOrTextStream
next

	| obj |
	obj := super next.
	self isBinary ifTrue: [ obj := obj asInteger ].
	^obj
%
category: 'accessing'
method: RWBinaryOrTextStream
next: count

"Returns the next count elements in the receiver's collection."

| result |
self isBinary
	ifTrue: [ result := ByteArray new ]
	ifFalse: [result := itsCollection species new ].
count timesRepeat: [ 
	self atEnd ifTrue: [ ^ result ].
	result add: self next ].
^result
%
category: 'accessing'
method: RWBinaryOrTextStream
next: n into: aCollection startingAt: startIndex
	"Read n objects into the given collection. 
	Return aCollection or a partial copy if less than
	n elements have been read."
	| obj |
	0 to: n-1 do:[:i|
		self atEnd ifTrue: [ ^aCollection copyFrom: 1 to: startIndex+i-1 ].
		obj := self next.
		self isBinary ifTrue: [ obj := obj asInteger ].
		aCollection at: startIndex+i put: obj].
	^aCollection
%
category: 'accessing'
method: RWBinaryOrTextStream
next: anInteger putAll: aCollection startingAt: startIndex
	"Store the next anInteger elements from the given collection."
	(startIndex = 1 and:[anInteger = aCollection size])
		ifTrue:[^self nextPutAll: aCollection].
	^self nextPutAll: (aCollection copyFrom: startIndex to: startIndex+anInteger-1)
%
category: 'Adding'
method: RWBinaryOrTextStream
nextPut: charOrByte

	super nextPut: charOrByte asCharacter
%
category: 'Adding'
method: RWBinaryOrTextStream
nextPutAll: aCollection
	^super nextPutAll: aCollection asString
%
category: 'converting'
method: RWBinaryOrTextStream
readStream
	"polymorphic with SequenceableCollection.  Return self"

	^ self
%
category: 'Positioning'
method: RWBinaryOrTextStream
reset
	"Set the receiver's position to the beginning of the sequence of objects."

	super reset.
	isBinary ifNil: [isBinary := false].
	itsCollection class == ByteArray ifTrue: ["Store as String and convert as needed."
		itsCollection := itsCollection asString.
		isBinary := true].
%
category: 'accessing'
method: RWBinaryOrTextStream
text

	isBinary := false
%
category: 'Positioning'
method: RWBinaryOrTextStream
_initStreamWith: aCollection

	super _initStreamWith: aCollection.
	isBinary := false
%

! Remove existing behavior from DataStream
removeallmethods DataStream
removeallclassmethods DataStream
! ------------------- Class methods for DataStream
category: 'instance creation'
classmethod: DataStream
on: aStream
	"Open a new DataStream onto a low-level I/O stream."

	^ self basicNew setStream: aStream
		"aStream binary is in setStream:"
%
! ------------------- Instance methods for DataStream
category: 'testing'
method: DataStream
atEnd
    "Answer true if the stream is at the end."

    ^ byteStream atEnd
%
category: 'Adding'
method: DataStream
beginInstance: aClass size: anInteger
	"This is for use by storeDataOn: methods.
	 Cf. Object>>storeDataOn:."

		"Addition of 1 seems to make extra work, since readInstance
		has to compensate.  Here for historical reasons dating back
		to Kent Beck's original implementation in late 1988.

		In ReferenceStream, class is just 5 bytes for shared symbol.

		SmartRefStream puts out the names and number of class's instances variables for checking."

	byteStream nextNumber: 4 put: anInteger + 1.

	self nextPut: aClass name
%
category: 'private'
method: DataStream
beginReference: anObject
    "We're starting to read anObject. Remember it and its reference
     position (if we care; ReferenceStream cares). Answer the
     reference position."

    ^ 0
%
category: 'private'
method: DataStream
getCurrentReference
    "PRIVATE -- Return the currentReference posn.
     Overridden by ReferenceStream."

    ^ 0
%
category: 'private'
method: DataStream
maybeBeginReference: internalObject
	"Do nothing.  See ReferenceStream|maybeBeginReference:"

	^ internalObject
%
category: 'accessing'
method: DataStream
next
	"Answer the next object in the stream."
	| type selector anObject isARefType pos internalObject |

	type := byteStream next.
	type ifNil: [pos := byteStream position.	"absolute!!"
		byteStream close.	"clean up"
		byteStream position = 0 
			ifTrue: [self error: 'The file did not exist in this directory'] 
			ifFalse: [self error: 'Unexpected end of object file'].
		^ nil].
	type = 0 ifTrue: [pos := byteStream position.	"absolute!!"
		byteStream close.	"clean up"
		self error: 'Expected start of object, but found 0'.
		^ nil].
	isARefType := self noteCurrentReference: type.
	selector := #(readNil readTrue readFalse readInteger	"<-4"
			readStringOld readSymbol readByteArray		"<-7"
			readArray readInstance readReference readBitmap	"<-11"
			readClass readUser readFloat readRectangle readShortInst 	"<-16"
			readString readWordArray readWordArrayForSegment 	"<-19"
			readWordLike readMethod "<-21") at: type.
	selector == 0 ifTrue: [pos := byteStream position.	"absolute!!"
			byteStream close. 
			self error: 'file is more recent than this system'. ^ nil].
	anObject := self perform: selector. "A method that recursively
		calls next (readArray, readInstance, objectAt:) must save &
		restore the current reference position."
	isARefType ifTrue: [self beginReference: anObject].

		"After reading the externalObject, internalize it.
		 #readReference is a special case. Either:
		   (1) We actually have to read the object, recursively calling
			   next, which internalizes the object.
		   (2) We just read a reference to an object already read and
			   thus already interalized.
		 Either way, we must not re-internalize the object here."
	selector == #readReference ifTrue: [^ anObject].
	internalObject := anObject comeFullyUpOnReload: self.
	^ self maybeBeginReference: internalObject
%
category: 'Adding'
method: DataStream
nextPut: anObject
	"Write anObject to the receiver stream. Answer anObject."
	| typeID selector objectToStore |

	typeID := self typeIDFor: anObject.
	(self tryToPutReference: anObject typeID: typeID)
		ifTrue: [^ anObject].

	objectToStore := (self objectIfBlocked: anObject) objectForDataStream: self.
	objectToStore == anObject ifFalse: [typeID := self typeIDFor: objectToStore].

	byteStream nextPut: typeID.
	selector := #(writeNil: writeTrue: writeFalse: writeInteger: 
		writeStringOld: writeSymbol: writeByteArray:
		writeArray: writeInstance: errorWriteReference: writeBitmap:
		writeClass: writeUser: writeFloat: writeRectangle: == "<-16 short inst" 
		writeString: writeBitmap: writeBitmap: writeWordLike: 
		writeInstance: "CompiledMethod") at: typeID.
	self perform: selector with: objectToStore.

	^ anObject


"NOTE: If anObject is a reference type (one that we write cross-references to) but its externalized form (result of objectForDataStream:) isn't (e.g. CompiledMethod and ViewState), then we should remember its externalized form
 but not add to 'references'. Putting that object again should just put its
 external form again. That's more compact and avoids seeks when reading.
 But we just do the simple thing here, allowing backward-references for
 non-reference types like nil. So objectAt: has to compensate. Objects that
 externalize nicely won't contain the likes of ViewStates, so this shouldn't
 hurt much.
	 writeReference: -> errorWriteReference:."
%
category: 'Adding'
method: DataStream
nextPutAll: aCollection
    "Write each of the objects in aCollection to the
     receiver stream. Answer aCollection."

    ^ aCollection do: [:each | self nextPut: each]
%
category: 'private'
method: DataStream
noteCurrentReference: typeID
    "PRIVATE -- If we support references for type typeID, remember
     the current byteStream position so we can add the next object to
     the 'objects' dictionary, and return true. Else return false.
     This method is here to be overridden by ReferenceStream"

    ^ false
%
category: 'private'
method: DataStream
objectIfBlocked: anObject
	"We don't do any blocking"

	^ anObject
%
category: 'private'
method: DataStream
readArray
	"PRIVATE -- Read the contents of an Array.
	 We must do beginReference: here after instantiating the Array
	 but before reading its contents, in case the contents reference
	 the Array. beginReference: will be sent again when we return to
	 next, but that's ok as long as we save and restore the current
	 reference position over recursive calls to next."
	| count array refPosn |

	count := byteStream nextNumber: 4.

	refPosn := self beginReference: (array := Array new: count).		"relative pos"
	1 to: count do: [:i |
		array at: i put: self next].
	self setCurrentReference: refPosn.		"relative pos"
	^ array
%
category: 'private'
method: DataStream
readFalse
    "PRIVATE -- Read the contents of a False."

    ^ false
%
category: 'private'
method: DataStream
readInstance
	"PRIVATE -- Read the contents of an arbitrary instance.
	 ASSUMES: readDataFrom:size: sends me beginReference: after it
	   instantiates the new object but before reading nested objects.
	 NOTE: We must restore the current reference position after
	   recursive calls to next.
	Let the instance, not the class read the data.  "
	| instSize aSymbol refPosn anObject newClass |

	instSize := (byteStream nextNumber: 4) - 1.
	refPosn := self getCurrentReference.
	aSymbol := self next.
	newClass := System myUserProfile symbolList objectNamed: aSymbol asSymbol.
	anObject := newClass isVariable 	"Create object here"
			ifFalse: [newClass basicNew]
			ifTrue: [newClass basicNew: instSize - (newClass instSize)].
	self setCurrentReference: refPosn.  "before readDataFrom:size:"
	anObject := anObject readDataFrom: self size: instSize.
	self setCurrentReference: refPosn.  "before returning to next"
	^ anObject
%
category: 'private'
method: DataStream
readInteger
    "PRIVATE -- Read the contents of a SmallInteger."

    ^ byteStream nextInt32	"signed!!!"
%
category: 'private'
method: DataStream
readNil
    "PRIVATE -- Read the contents of an UndefinedObject."

    ^ nil
%
category: 'private'
method: DataStream
readString

	| str |
	byteStream ascii.
	str := byteStream nextString.
	byteStream binary.
	^ str
%
category: 'private'
method: DataStream
readSymbol
    "PRIVATE -- Read the contents of a Symbol."

    ^ self readString asSymbol
%
category: 'private'
method: DataStream
readTrue
    "PRIVATE -- Read the contents of a True."

    ^ true
%
category: 'private'
method: DataStream
setCurrentReference: refPosn
    "PRIVATE -- Set currentReference to refPosn.
     Noop here. Cf. ReferenceStream."
%
category: 'initialization'
method: DataStream
setStream: aStream
	"PRIVATE -- Initialization method."

	aStream binary.
	basePos := aStream position.	"Remember where we start.  Earlier part of file contains a class or method file-in.  Allow that to be edited.  We don't deal in absolute file locations."
	byteStream := aStream.
%
category: 'private'
method: DataStream
tryToPutReference: anObject typeID: typeID
    "PRIVATE -- If we support references for type typeID, and if
       anObject already appears in my output stream, then put a
       reference to the place where anObject already appears. If we
       support references for typeID but didn't already put anObject,
       then associate the current stream position with anObject in
       case one wants to nextPut: it again.
     Return true after putting a reference; false if the object still
       needs to be put.
     For DataStream this is trivial. ReferenceStream overrides this."

    ^ false
%
category: 'private'
method: DataStream
typeIDFor: anObject
	"Return the typeID for anObject's class.  This is where the tangle of objects is clipped to stop everything from going out.  
	Classes can control their instance variables by defining objectToStoreOnDataStream.
	Any object in blockers is not written out.  See ReferenceStream.objectIfBlocked: and DataStream nextPut:.
	Morphs do not write their owners.  See Morph.storeDataOn:   Each morph tells itself to 'prepareToBeSaved' before writing out."
	
	(anObject isKindOf: Boolean) ifTrue: [
		anObject ifTrue: [ ^2 ]. "true"
		^3 "false"
	].
	^ TypeMap at: anObject class ifAbsent: [9 "instance of any normal class"]	
"See DataStream initialize.  nil=1. true=2. false=3. a SmallInteger=4. (a String was 5). a Symbol=6.  a ByteArray=7. an Array=8. other = 9.  a Bitmap=11. a Metaclass=12. a Float=14.  a Rectangle=15. any instance that can have a short header=16.  a String=17 (new format). a WordArray=18."
%
category: 'private'
method: DataStream
writeArray: anArray
	"PRIVATE -- Write the contents of an Array."

	byteStream nextNumber: 4 put: anArray size.
	self nextPutAll: anArray.
%
category: 'private'
method: DataStream
writeFalse: aFalse
    "PRIVATE -- Write the contents of a False."
%
category: 'private'
method: DataStream
writeInstance: anObject
    "PRIVATE -- Write the contents of an arbitrary instance."

    ^ anObject storeDataOn: self
%
category: 'private'
method: DataStream
writeInteger: anInteger
	"PRIVATE -- Write the contents of a SmallInteger."

	byteStream nextInt32Put: anInteger	"signed!!!!!"
%
category: 'private'
method: DataStream
writeNil: anUndefinedObject
    "PRIVATE -- Write the contents of an UndefinedObject."
%
category: 'private'
method: DataStream
writeString: aString
	"PRIVATE -- Write the contents of a String."

	byteStream nextStringPut: aString.
%
category: 'private'
method: DataStream
writeSymbol: aSymbol
    "PRIVATE -- Write the contents of a Symbol."

    self writeString: aSymbol
%
category: 'private'
method: DataStream
writeTrue: aTrue
    "PRIVATE -- Write the contents of a True."
%

! Remove existing behavior from ReferenceStream
removeallmethods ReferenceStream
removeallclassmethods ReferenceStream
! ------------------- Class methods for ReferenceStream
category: 'accessing'
classmethod: ReferenceStream
refTypes: oc
	RefTypes := oc
%
! ------------------- Instance methods for ReferenceStream

! Remove existing behavior from ZipEncoder
removeallmethods ZipEncoder
removeallclassmethods ZipEncoder
! ------------------- Class methods for ZipEncoder
! ------------------- Instance methods for ZipEncoder
category: 'accessing'
method: ZipEncoder
bitPosition
	^encodedStream position + position - 2 * 8 + bitPosition.
%
category: 'initialize-release'
method: ZipEncoder
commit
	encodedStream nextPutAll: (itsCollection copyFrom: 1 to: position - 1).
	position :=  1.
%
category: 'initialize-release'
method: ZipEncoder
flush
	self flushBits.
	self commit.
%
category: 'initialize-release'
method: ZipEncoder
flushBits
	"Flush currently unsent bits"
	[bitPosition > 0] whileTrue:[
		self nextBytePut: (bitBuffer bitAnd: 255).
		bitBuffer := bitBuffer bitShift: -8.
		bitPosition := bitPosition - 8].
	bitPosition := 0.
%
category: 'accessing'
method: ZipEncoder
nextBits: nBits put: value
	"Store a value of nBits"
	"self assert:[value >= 0 and:[(1 bitShift: nBits) > value]]."
	bitBuffer := bitBuffer bitOr: (value bitShift: bitPosition).
	bitPosition := bitPosition + nBits.
	[bitPosition >= 8] whileTrue:[
		self nextBytePut: (bitBuffer bitAnd: 255).
		bitBuffer := bitBuffer bitShift: -8.
		bitPosition := bitPosition - 8].
%
category: 'accessing'
method: ZipEncoder
nextBytePut: anObject 
	"Insert the argument at the next position in the Stream
	represented by the receiver."

	isBinary 
		ifTrue: [ self nextPut: anObject asInteger ]
		ifFalse: [self nextPut: anObject asCharacter ]
%
category: 'private'
method: ZipEncoder
privateSendBlock: literalStream with: distanceStream with: litTree with: distTree
	"Send the current block using the encodings from the given literal/length and distance tree"
	| lit dist code extra sum |
	sum := 0.
	[ literalStream atEnd ] whileFalse:[
		lit := literalStream next.
		dist := distanceStream next.
		dist = 0 ifTrue:["lit is a literal"
			sum := sum + 1.
			self nextBits: (litTree bitLengthAt: lit)
				put: (litTree codeAt: lit).
		] ifFalse:["lit is match length"
			sum := sum + lit + MinMatch.
			code := (MatchLengthCodes at: lit + 1).
			self nextBits: (litTree bitLengthAt: code)
				put: (litTree codeAt: code).
			extra := ExtraLengthBits at: code-NumLiterals.
			extra = 0 ifFalse:[
				lit := lit - (BaseLength at: code-NumLiterals).
				self nextBits: extra put: lit.
			].
			dist := dist - 1.
			dist < 256
				ifTrue:[code := DistanceCodes at: dist + 1]
				ifFalse:[code := DistanceCodes at: 257 + (dist bitShift: -7)].
			"self assert:[code < MaxDistCodes]."
			self nextBits: (distTree bitLengthAt: code)
				put: (distTree codeAt: code).
			extra := ExtraDistanceBits at: code+1.
			extra = 0 ifFalse:[
				dist := dist - (BaseDistance at: code+1).
				self nextBits: extra put: dist.
			].
		].
	].
	^sum
%
category: 'block encoding'
method: ZipEncoder
sendBlock: literalStream with: distanceStream with: litTree with: distTree
	"Send the current block using the encodings from the given literal/length and distance tree"
	| result |
	result := 0.
	[literalStream atEnd] whileFalse:[
		result := result + (self privateSendBlock: literalStream
						with: distanceStream with: litTree with: distTree).
		self commit.
	].
	self nextBits: (litTree bitLengthAt: EndBlock) put: (litTree codeAt: EndBlock).
	^result
%
category: 'Positioning'
method: ZipEncoder
_initStreamWith: aCollectionOrStream
	aCollectionOrStream isStream 
		ifTrue:[encodedStream := aCollectionOrStream]
		ifFalse:[	encodedStream := WriteStream on: aCollectionOrStream].
	encodedStream isBinary
		ifTrue:[
			isBinary := true.
			super _initStreamWith: (ByteArray new: 4096).
		]
		ifFalse:[
			isBinary := false.
			super _initStreamWith: (String new: 4096).
		].
	bitPosition := bitBuffer := 0.
%

! Remove existing behavior from ZipWriteStream
removeallmethods ZipWriteStream
removeallclassmethods ZipWriteStream
! ------------------- Class methods for ZipWriteStream
category: 'initialization'
classmethod: ZipWriteStream
initialize
	"ZipWriteStream initialize"
	VerboseLevel := 0.
	self initializeCrcTable.
%
category: 'initialization'
classmethod: ZipWriteStream
initializeCrcTable
	"ZipWriteStream initialize"
	CrcTable := #(16r00000000 16r77073096 16rEE0E612C 16r990951BA 16r076DC419
  16r706AF48F 16rE963A535 16r9E6495A3 16r0EDB8832 16r79DCB8A4
  16rE0D5E91E 16r97D2D988 16r09B64C2B 16r7EB17CBD 16rE7B82D07
  16r90BF1D91 16r1DB71064 16r6AB020F2 16rF3B97148 16r84BE41DE
  16r1ADAD47D 16r6DDDE4EB 16rF4D4B551 16r83D385C7 16r136C9856
  16r646BA8C0 16rFD62F97A 16r8A65C9EC 16r14015C4F 16r63066CD9
  16rFA0F3D63 16r8D080DF5 16r3B6E20C8 16r4C69105E 16rD56041E4
  16rA2677172 16r3C03E4D1 16r4B04D447 16rD20D85FD 16rA50AB56B
  16r35B5A8FA 16r42B2986C 16rDBBBC9D6 16rACBCF940 16r32D86CE3
  16r45DF5C75 16rDCD60DCF 16rABD13D59 16r26D930AC 16r51DE003A
  16rC8D75180 16rBFD06116 16r21B4F4B5 16r56B3C423 16rCFBA9599
  16rB8BDA50F 16r2802B89E 16r5F058808 16rC60CD9B2 16rB10BE924
  16r2F6F7C87 16r58684C11 16rC1611DAB 16rB6662D3D 16r76DC4190
  16r01DB7106 16r98D220BC 16rEFD5102A 16r71B18589 16r06B6B51F
  16r9FBFE4A5 16rE8B8D433 16r7807C9A2 16r0F00F934 16r9609A88E
  16rE10E9818 16r7F6A0DBB 16r086D3D2D 16r91646C97 16rE6635C01
  16r6B6B51F4 16r1C6C6162 16r856530D8 16rF262004E 16r6C0695ED
  16r1B01A57B 16r8208F4C1 16rF50FC457 16r65B0D9C6 16r12B7E950
  16r8BBEB8EA 16rFCB9887C 16r62DD1DDF 16r15DA2D49 16r8CD37CF3
  16rFBD44C65 16r4DB26158 16r3AB551CE 16rA3BC0074 16rD4BB30E2
  16r4ADFA541 16r3DD895D7 16rA4D1C46D 16rD3D6F4FB 16r4369E96A
  16r346ED9FC 16rAD678846 16rDA60B8D0 16r44042D73 16r33031DE5
  16rAA0A4C5F 16rDD0D7CC9 16r5005713C 16r270241AA 16rBE0B1010
  16rC90C2086 16r5768B525 16r206F85B3 16rB966D409 16rCE61E49F
  16r5EDEF90E 16r29D9C998 16rB0D09822 16rC7D7A8B4 16r59B33D17
  16r2EB40D81 16rB7BD5C3B 16rC0BA6CAD 16rEDB88320 16r9ABFB3B6
  16r03B6E20C 16r74B1D29A 16rEAD54739 16r9DD277AF 16r04DB2615
  16r73DC1683 16rE3630B12 16r94643B84 16r0D6D6A3E 16r7A6A5AA8
  16rE40ECF0B 16r9309FF9D 16r0A00AE27 16r7D079EB1 16rF00F9344
  16r8708A3D2 16r1E01F268 16r6906C2FE 16rF762575D 16r806567CB
  16r196C3671 16r6E6B06E7 16rFED41B76 16r89D32BE0 16r10DA7A5A
  16r67DD4ACC 16rF9B9DF6F 16r8EBEEFF9 16r17B7BE43 16r60B08ED5
  16rD6D6A3E8 16rA1D1937E 16r38D8C2C4 16r4FDFF252 16rD1BB67F1
  16rA6BC5767 16r3FB506DD 16r48B2364B 16rD80D2BDA 16rAF0A1B4C
  16r36034AF6 16r41047A60 16rDF60EFC3 16rA867DF55 16r316E8EEF
  16r4669BE79 16rCB61B38C 16rBC66831A 16r256FD2A0 16r5268E236
  16rCC0C7795 16rBB0B4703 16r220216B9 16r5505262F 16rC5BA3BBE
  16rB2BD0B28 16r2BB45A92 16r5CB36A04 16rC2D7FFA7 16rB5D0CF31
  16r2CD99E8B 16r5BDEAE1D 16r9B64C2B0 16rEC63F226 16r756AA39C
  16r026D930A 16r9C0906A9 16rEB0E363F 16r72076785 16r05005713
  16r95BF4A82 16rE2B87A14 16r7BB12BAE 16r0CB61B38 16r92D28E9B
  16rE5D5BE0D 16r7CDCEFB7 16r0BDBDF21 16r86D3D2D4 16rF1D4E242
  16r68DDB3F8 16r1FDA836E 16r81BE16CD 16rF6B9265B 16r6FB077E1
  16r18B74777 16r88085AE6 16rFF0F6A70 16r66063BCA 16r11010B5C
  16r8F659EFF 16rF862AE69 16r616BFFD3 16r166CCF45 16rA00AE278
  16rD70DD2EE 16r4E048354 16r3903B3C2 16rA7672661 16rD06016F7
  16r4969474D 16r3E6E77DB 16rAED16A4A 16rD9D65ADC 16r40DF0B66
  16r37D83BF0 16rA9BCAE53 16rDEBB9EC5 16r47B2CF7F 16r30B5FFE9
  16rBDBDF21C 16rCABAC28A 16r53B39330 16r24B4A3A6 16rBAD03605
  16rCDD70693 16r54DE5729 16r23D967BF 16rB3667A2E 16rC4614AB8
  16r5D681B02 16r2A6F2B94 16rB40BBE37 16rC30C8EA1 16r5A05DF1B
  16r2D02EF8D
).
%
category: 'crc'
classmethod: ZipWriteStream
updateCrc: oldCrc from: start to: stop in: aCollection
	| newCrc |
	newCrc := oldCrc.
	start to: stop do:[:i|
		newCrc := (CrcTable at: ((newCrc bitXor: (aCollection byteAt: i)) 
				bitAnd: 255) + 1) bitXor: (newCrc bitShift: -8).
	].
	^newCrc
%
! ------------------- Instance methods for ZipWriteStream
category: 'deflating'
method: ZipWriteStream
compare: here with: matchPos min: minLength
	"Compare the two strings and return the length of matching characters.
	minLength is a lower bound for match lengths that will be accepted.
	Note: here and matchPos are zero based."
	| length |
	"First test if we can actually get longer than minLength"
	(itsCollection at: here+minLength+1) = (itsCollection at: matchPos+minLength+1)
		ifFalse:[^0].
	(itsCollection at: here+minLength) = (itsCollection at: matchPos+minLength)
		ifFalse:[^0].
	"Then test if we have an initial match at all"
	(itsCollection at: here+1) = (itsCollection at: matchPos+1)
		ifFalse:[^0].
	(itsCollection at: here+2) = (itsCollection at: matchPos+2)
		ifFalse:[^1].
	"Finally do the real comparison"
	length := 3.
	[length <= MaxMatch and:[
		(itsCollection at: here+length) = (itsCollection at: matchPos+length)]]
			whileTrue:[length := length + 1].
	^length - 1
%
category: 'accessing'
method: ZipWriteStream
crc
	^crc
%
category: 'deflating'
method: ZipWriteStream
deflateBlock
	"Deflate the current contents of the stream"
	| flushNeeded lastIndex |
	(blockStart == nil) ifTrue:[
		"One time initialization for the first block"
		1 to: MinMatch-1 do:[:i| self updateHashAt: i].
		blockStart := 0].

	[blockPosition < (position - 1)] whileTrue:[
		(position + MaxMatch > writeLimit)
			ifTrue:[lastIndex := writeLimit - MaxMatch]
			ifFalse:[lastIndex := position - 1].
		flushNeeded := self deflateBlock: lastIndex-1
							chainLength: self hashChainLength
							goodMatch: self goodMatchLength.
		flushNeeded ifTrue:[
			self flushBlock.
			blockStart := blockPosition].
		"Make room for more data"
		self moveContentsToFront].
%
category: 'deflating'
method: ZipWriteStream
deflateBlock: lastIndex chainLength: chainLength goodMatch: goodMatch
	"Continue deflating the receiver's collection from blockPosition to lastIndex.
	Note that lastIndex must be at least MaxMatch away from the end of collection"
	| here matchResult flushNeeded hereMatch hereLength newMatch newLength hasMatch |
	blockPosition > lastIndex ifTrue:[^false]. "Nothing to deflate"
	hasMatch := false.
	here := blockPosition.
	[here <= lastIndex] whileTrue:[
		hasMatch ifFalse:[
			"Find the first match"
			matchResult := self findMatch: here
								lastLength: MinMatch-1
								lastMatch: here
								chainLength: chainLength
								goodMatch: goodMatch.
			self insertStringAt: here. "update hash table"
			hereMatch := matchResult bitAnd: 16rFFFF.
			hereLength := matchResult bitShift: -16].

		"Look ahead if there is a better match at the next position"
		matchResult := self findMatch: here+1
							lastLength: hereLength
							lastMatch: hereMatch
							chainLength: chainLength
							goodMatch: goodMatch.
		newMatch := matchResult bitAnd: 16rFFFF.
		newLength := matchResult bitShift: -16.

		"Now check if the next match is better than the current one.
		If not, output the current match (provided that the current match
		is at least MinMatch long)"
		((hereLength >= newLength and:[hereLength >= MinMatch]) and: [ here + hereLength < lastIndex ]) ifTrue:[
			self assert:[self validateMatchAt: here
							from: hereMatch to: hereMatch + hereLength - 1].
			"Encode the current match"
			flushNeeded := self
				encodeMatch: hereLength
				distance: here - hereMatch.
			"Insert all strings up to the end of the current match.
			Note: The first string has already been inserted."
			1 to: hereLength-1 do:[:i| self insertStringAt: (here := here + 1)].
			hasMatch := false.
			here := here + 1.
		] ifFalse:[
			"Either the next match is better than the current one or we didn't
			have a good match after all (e.g., current match length < MinMatch).
			Output a single literal."
			flushNeeded := self encodeLiteral: (itsCollection byteAt: (here + 1)).
			here := here + 1.
			(here <= lastIndex and:[flushNeeded not]) ifTrue:[
				"Cache the results for the next round"
				self insertStringAt: here.
				hasMatch := true.
				hereMatch := newMatch.
				hereLength := newLength].
		].
		flushNeeded ifTrue:[blockPosition := here. ^true].
	].
	blockPosition := here.
	^false
%
category: 'dynamic blocks'
method: ZipWriteStream
dynamicBlockSizeFor: lTree and: dTree using: blTree and: blFreq
	"Compute the length for the current block using dynamic huffman trees"
	| bits index extra treeBits freq |
	bits := 3 "block type" + 5 "literal codes length" + 5 "distance codes length".

	"Compute the # of bits for sending the bit length tree"
	treeBits := 4. "Max index for bit length tree"
	index := MaxBitLengthCodes.
	[index >= 4] whileTrue:[
		(index = 4 or:[(blFreq at: (BitLengthOrder at: index)+1) > 0])
			ifTrue:[treeBits := treeBits + (index * 3).
					index := -1]
			ifFalse:[index := index - 1]].

	"Compute the # of bits for sending the literal/distance tree.
	Note: The frequency are already stored in the blTree"
	0 to: 15 do:[:i| "First, the non-repeating values"
		freq := blFreq at: i+1.
		freq > 0 ifTrue:[treeBits := treeBits + (freq * (blTree bitLengthAt: i))]].
	"Now the repeating values"
	(Repeat3To6 to: Repeat11To138) with: #(2 3 7) do:[:i :addl|
		freq := blFreq at: i+1.
		freq > 0 ifTrue:[
			treeBits := treeBits + (freq * ((blTree bitLengthAt: i) + addl "addl bits"))]].
	VerboseLevel > 1 ifTrue:[
		"Transcript show:'['; print: treeBits; show:' bits for dynamic tree]'"].
	bits := bits + treeBits.

	"Compute the size of the compressed block"
	0 to: NumLiterals do:[:i| "encoding of literals"
		freq := literalFreq at: i+1.
		freq > 0 ifTrue:[bits := bits + (freq * (lTree bitLengthAt: i))]].
	NumLiterals+1 to: lTree maxCode do:[:i| "encoding of match lengths"
		freq := literalFreq at: i+1.
		extra := ExtraLengthBits at: i-NumLiterals.
		freq > 0 ifTrue:[bits := bits + (freq * ((lTree bitLengthAt: i) + extra))]].
	0 to: dTree maxCode do:[:i| "encoding of distances"
		freq := distanceFreq at: i+1.
		extra := ExtraDistanceBits at: i+1.
		freq > 0 ifTrue:[bits := bits + (freq * ((dTree bitLengthAt: i) + extra))]].

	^bits
%
category: 'encoding'
method: ZipWriteStream
encodeLiteral: lit
	"Encode the given literal"
	litCount := litCount + 1.
	literals at: litCount put: lit.
	distances at: litCount put: 0.
	literalFreq at: lit+1 put: (literalFreq at: lit+1) + 1.
	^self shouldFlush
%
category: 'encoding'
method: ZipWriteStream
encodeMatch: length distance: dist
	"Encode the given match of length length starting at dist bytes ahead"
	| literal distance |
	dist > 0 
		ifFalse:[^self error:'Distance must be positive'].
	length < MinMatch 
		ifTrue:[^self error:'Match length must be at least ', MinMatch printString].
	litCount := litCount + 1.
	matchCount := matchCount + 1.
	literals at: litCount put: length - MinMatch.
	distances at: litCount put: dist.
	literal := (MatchLengthCodes at: length - MinMatch + 1).
	literalFreq at: literal+1 put: (literalFreq at: literal+1) + 1.
	dist < 257
		ifTrue:[distance := DistanceCodes at: dist]
		ifFalse:[distance := DistanceCodes at: 257 + (dist - 1 bitShift: -7)].
	distanceFreq at: distance+1 put: (distanceFreq at: distance+1) + 1.
	^self shouldFlush
%
category: 'deflating'
method: ZipWriteStream
findMatch: here lastLength: lastLength lastMatch: lastMatch chainLength: maxChainLength goodMatch: goodMatch
	"Find the longest match for the string starting at here.
	If there is no match longer than lastLength return lastMatch/lastLength.
	Traverse at most maxChainLength entries in the hash table.
	Stop if a match of at least goodMatch size has been found."
	| matchResult matchPos distance chainLength limit bestLength length |
	"Compute the default match result"
	matchResult := (lastLength bitShift: 16) bitOr: lastMatch.

	"There is no way to find a better match than MaxMatch"
	lastLength >= MaxMatch ifTrue:[^matchResult].

	"Start position for searches"
	matchPos := hashHead at: (self updateHashAt: here + MinMatch) + 1.

	"Compute the distance to the (possible) match"
	distance := here - matchPos.

	"Note: It is required that 0 < distance < MaxDistance"
	(distance > 0 and:[distance < MaxDistance]) ifFalse:[^matchResult].

	chainLength := maxChainLength.	"Max. nr of match chain to search"
	here > MaxDistance	"Limit for matches that are too old"
		ifTrue:[limit := here - MaxDistance]
		ifFalse:[limit := 0].

	"Best match length so far (current match must be larger to take effect)"
	bestLength := lastLength.

	["Compare the current string with the string at match position"
	length := self compare: here with: matchPos min: bestLength.
	"Truncate accidental matches beyound stream position"
	(here + length > position) ifTrue:[length := position - here].
	"Ignore very small matches if they are too far away"
	(length = MinMatch and:[(here - matchPos) > (MaxDistance // 4)])
		ifTrue:[length := MinMatch - 1].
	length > bestLength ifTrue:["We have a new (better) match than before"
		"Compute the new match result"
		matchResult := (length bitShift: 16) bitOr: matchPos.
		bestLength := length.
		"There is no way to find a better match than MaxMatch"
		bestLength >= MaxMatch ifTrue:[^matchResult].
		"But we may have a good, fast match"
		bestLength > goodMatch ifTrue:[^matchResult].
	].
	(chainLength := chainLength - 1) > 0] whileTrue:[
		"Compare with previous entry in hash chain"
		matchPos := hashTail at: (matchPos bitAnd: WindowMask) + 1.
		matchPos <= limit ifTrue:[^matchResult]. "Match position is too old"
	].
	^matchResult
%
category: 'initialize-release'
method: ZipWriteStream
finish
	"Finish pending operation. Do not close output stream."
	self deflateBlock.
	self flushBlock: true.
	encoder flush.
%
category: 'fixed blocks'
method: ZipWriteStream
fixedBlockSizeFor: lTree and: dTree
	"Compute the length for the current block using fixed huffman trees"
	| bits extra |
	bits := 3 "block type".
	"Compute the size of the compressed block"
	0 to: NumLiterals do:[:i| "encoding of literals"
		bits := bits + ((literalFreq at: i+1) * (FixedLiteralTree bitLengthAt: i))].
	NumLiterals+1 to: lTree maxCode+1 do:[:i| "Encoding of match lengths"
		extra := ExtraLengthBits at: i-NumLiterals.
		bits := bits + ((literalFreq at: i+1) * ((FixedLiteralTree bitLengthAt: i) + extra))].
	0 to: dTree maxCode do:[:i| "encoding of distances"
		extra := ExtraDistanceBits at: i+1.
		bits := bits + ((distanceFreq at: i+1) * ((FixedDistanceTree bitLengthAt: i) + extra))].

	^bits
%
category: 'encoding'
method: ZipWriteStream
flushBlock
	^self flushBlock: false
%
category: 'encoding'
method: ZipWriteStream
flushBlock: lastBlock
	"Send the current block"
	| lastFlag bitsRequired method bitsSent
	storedLength fixedLength dynamicLength 
	blTree lTree dTree blBits blFreq |

	lastFlag := lastBlock ifTrue:[1] ifFalse:[0].

	"Compute the literal/length and distance tree"
	lTree := ZipEncoderTree buildTreeFrom: literalFreq maxDepth: MaxBits.
	dTree := ZipEncoderTree buildTreeFrom: distanceFreq maxDepth: MaxBits.

	"Compute the bit length tree"
	blBits := lTree bitLengths, dTree bitLengths.
	blFreq := WordArray new: MaxBitLengthCodes.
	self scanBitLengths: blBits into: blFreq.
	blTree := ZipEncoderTree buildTreeFrom: blFreq maxDepth: MaxBitLengthBits.

	"Compute the bit length for the current block.
	Note: Most of this could be computed on the fly but it's getting
	really ugly in this case so we do it afterwards."
	storedLength := self storedBlockSize.
	fixedLength := self fixedBlockSizeFor: lTree and: dTree.
	dynamicLength := self dynamicBlockSizeFor: lTree and: dTree 
							using: blTree and: blFreq.

	"Check which method to use"
	method := self forcedMethod.
	method == nil ifTrue:[
		method := (storedLength < fixedLength and:[storedLength < dynamicLength]) 
			ifTrue:[#stored]
			ifFalse:[fixedLength < dynamicLength ifTrue:[#fixed] ifFalse:[#dynamic]]].
	(method == #stored and:[blockStart < 0]) ifTrue:[
		"Cannot use #stored if the block is not available"
		method := fixedLength < dynamicLength ifTrue:[#fixed] ifFalse:[#dynamic]].

	bitsSent := encoder bitPosition. "# of bits sent before this block"
	bitsRequired := nil.

	(method == #stored) ifTrue:[
		bitsRequired := storedLength.
		encoder nextBits: 3 put: StoredBlock << 1 + lastFlag.
		self sendStoredBlock].

	(method == #fixed) ifTrue:[
		bitsRequired := fixedLength.
		encoder nextBits: 3 put: FixedBlock << 1 + lastFlag.
		self sendFixedBlock].

	(method == #dynamic) ifTrue:[
		bitsRequired := dynamicLength.
		encoder nextBits: 3 put: DynamicBlock << 1 + lastFlag.
		self sendDynamicBlock: blTree 
			literalTree: lTree 
			distanceTree: dTree 
			bitLengths: blBits].

	bitsRequired = (encoder bitPosition - bitsSent)
		ifFalse:[self error:'Bits size mismatch'].

	lastBlock 
		ifTrue:[self release]
		ifFalse:[self initializeNewBlock].
%
category: 'accessing'
method: ZipWriteStream
forcedMethod
	"Return a symbol describing an enforced method or nil if the method should
	be chosen adaptively. Valid symbols are
		#stored	- store blocks (do not compress)
		#fixed	- use fixed huffman trees
		#dynamic	- use dynamic huffman trees."
	^nil
%
category: 'accessing'
method: ZipWriteStream
goodMatchLength
	"Return the length that is considered to be a 'good' match.
	Higher values will result in better compression but take more time."
	^MaxMatch "Best compression"
%
category: 'accessing'
method: ZipWriteStream
hashChainLength
	"Return the max. number of hash chains to traverse.
	Higher values will result in better compression but take more time."
	^4096 "Best compression"
%
category: 'initialize-release'
method: ZipWriteStream
initialize
	literals := ByteArray new: WindowSize.
	distances := WordArray new: WindowSize.
	literalFreq := WordArray new: MaxLiteralCodes.
	distanceFreq := WordArray new: MaxDistCodes.
	self initializeNewBlock.
	blockStart = nil.
	blockPosition := 0.
	hashValue := 0.
	self initializeHashTables.
%
category: 'initialize-release'
method: ZipWriteStream
initializeHashTables
	hashHead := WordArray new: 1 << HashBits.
	hashTail := WordArray new: WindowSize.
%
category: 'initialize-release'
method: ZipWriteStream
initializeNewBlock
	"Initialize the encoder for a new block of data"
	literalFreq atAllPut: 0.
	distanceFreq atAllPut: 0.
	literalFreq at: EndBlock+1 put: 1.
	litCount := 0.
	matchCount := 0.
%
category: 'deflating'
method: ZipWriteStream
insertStringAt: here
	"Insert the string at the given start position into the hash table.
	Note: The hash value is updated starting at MinMatch-1 since
	all strings before have already been inserted into the hash table
	(and the hash value is updated as well)."
	| prevEntry |
	hashValue := self updateHashAt: (here + MinMatch).
	prevEntry := hashHead at: hashValue+1.
	hashHead at: hashValue+1 put: here.
	hashTail at: (here bitAnd: WindowMask)+1 put: prevEntry.
%
category: 'private'
method: ZipWriteStream
moveContentsToFront
	"Need to update crc here"
	| delta |
	self updateCrc.
	delta := (blockPosition - WindowSize).
	delta <= 0 ifTrue:[^self].
	"Move collection"
	itsCollection 
		squeakReplaceFrom: 1 
		to: itsCollection size - delta 
		with: itsCollection 
		startingAt: delta+1.
	position := position - delta.
	"Move hash table entries"
	blockPosition := blockPosition - delta.
	blockStart := blockStart - delta.
	self updateHashTable: hashHead delta: delta.
	self updateHashTable: hashTail delta: delta.
	crcPosition := position.
%
category: 'accessing'
method: ZipWriteStream
next: bytes putAll: aCollection startingAt: startPos
	(startPos = 1 and:[bytes = aCollection size]) 
		ifTrue:[^self nextPutAll: aCollection].
	^self nextPutAll: (aCollection copyFrom: startPos to: startPos + bytes - 1)
%
category: 'accessing'
method: ZipWriteStream
nextPut: anObject

	position <= itsCollection size
		ifTrue: [ ^ super nextPut: anObject ].
	^self pastEndPut: anObject
%
category: 'accessing'
method: ZipWriteStream
nextPutAll: aCollection
	aCollection do:[:ch| self nextPut: ch].
	^aCollection.
%
category: 'accessing'
method: ZipWriteStream
pastEndPut: anObject
	self deflateBlock.
	^self nextPut: anObject
%
category: 'initialize-release'
method: ZipWriteStream
release
	"We're done with compression. Do some cleanup."
	literals := distances := literalFreq := distanceFreq := nil.
	self updateCrc.
	encoder flushBits.
	self writeFooter.
%
category: 'dynamic blocks'
method: ZipWriteStream
scanBitLength: bitLength repeatCount: repeatCount into: anArray
	"Update the frequency for the aTree based on the given values"
	| count |
	count := repeatCount.
	bitLength = 0 ifTrue:[
		[count >= 11] whileTrue:[
			anArray at: Repeat11To138+1 put: (anArray at: Repeat11To138+1) + 1.
			count := (count - 138) max: 0].
		[count >= 3] whileTrue:[
			anArray at: Repeat3To10+1 put: (anArray at: Repeat3To10+1) + 1.
			count := (count - 10) max: 0].
		count > 0 ifTrue:[anArray at: bitLength+1 put: (anArray at: bitLength+1) + count].
	] ifFalse:[
		anArray at: bitLength+1 put: (anArray at: bitLength+1) + 1.
		count := count - 1.
		[count >= 3] whileTrue:[
			anArray at: Repeat3To6+1 put: (anArray at: Repeat3To6+1) + 1.
			count := (count - 6) max: 0].
		count > 0 ifTrue:[anArray at: bitLength+1 put: (anArray at: bitLength+1) + count].
	].
%
category: 'dynamic blocks'
method: ZipWriteStream
scanBitLengths: bits into: anArray
	"Scan the trees and determine the frequency of the bit lengths.
	For repeating codes, emit a repeat count."
	| lastValue lastCount value |
	bits size = 0 ifTrue:[^self].
	lastValue := bits at: 1.
	lastCount := 1.
	2 to: bits size do:[:i|
		value := bits at: i.
		value = lastValue 
			ifTrue:[lastCount := lastCount + 1]
			ifFalse:[self scanBitLength: lastValue repeatCount: lastCount into: anArray.
					lastValue := value.
					lastCount := 1]].
	self scanBitLength: lastValue repeatCount: lastCount into: anArray.
%
category: 'dynamic blocks'
method: ZipWriteStream
sendBitLength: bitLength repeatCount: repeatCount tree: aTree
	"Send the given bitLength, repeating repeatCount times"
	| count |
	count := repeatCount.
	bitLength = 0 ifTrue:[
		[count >= 11] whileTrue:[
			self sendBitLength: Repeat11To138 tree: aTree.
			encoder nextBits: 7 put: (count min: 138) - 11.
			count := (count - 138) max: 0].
		[count >= 3] whileTrue:[
			self sendBitLength: Repeat3To10 tree: aTree.
			encoder nextBits: 3 put: (count min: 10) - 3.
			count := (count - 10) max: 0].
		count timesRepeat:[self sendBitLength: bitLength tree: aTree].
	] ifFalse:[
		self sendBitLength: bitLength tree: aTree.
		count := count - 1.
		[count >= 3] whileTrue:[
			self sendBitLength: Repeat3To6 tree: aTree.
			encoder nextBits: 2 put: (count min: 6) - 3.
			count := (count - 6) max: 0].
		count timesRepeat:[self sendBitLength: bitLength tree: aTree].
	].
%
category: 'dynamic blocks'
method: ZipWriteStream
sendBitLength: bitLength tree: aTree
	"Send the given bitLength"
	encoder nextBits: (aTree bitLengthAt: bitLength) 
		put: (aTree codeAt: bitLength).
%
category: 'dynamic blocks'
method: ZipWriteStream
sendBitLengthTree: blTree
	"Send the bit length tree"
	| blIndex bitLength |
	MaxBitLengthCodes to: 4 by: -1 do:[:maxIndex|
		blIndex := BitLengthOrder at: maxIndex.
		bitLength := blIndex <= blTree maxCode 
			ifTrue:[blTree bitLengthAt: blIndex] ifFalse:[0].
		(maxIndex = 4 or:[bitLength > 0]) ifTrue:[
			encoder nextBits: 4 put: maxIndex - 4.
			1 to: maxIndex do:[:j|
				blIndex := BitLengthOrder at: j.
				bitLength := blIndex <= blTree maxCode 
					ifTrue:[blTree bitLengthAt: blIndex] ifFalse:[0].
				encoder nextBits: 3 put: bitLength].
			^self]].
%
category: 'dynamic blocks'
method: ZipWriteStream
sendCompressedBlock: litTree with: distTree
	"Send the current block using the encodings from the given literal/length and distance tree"
	| sum |
	sum := encoder
			sendBlock: (ReadStream on: literals from: 1 to: litCount)
			with: (ReadStream on: distances from: 1 to: litCount)
			with: litTree
			with: distTree.
	sum = (blockPosition - blockStart) ifFalse:[self error:'Wrong number of bytes'].
%
category: 'dynamic blocks'
method: ZipWriteStream
sendDynamicBlock: blTree literalTree: lTree distanceTree: dTree bitLengths: bits
	"Send a block using dynamic huffman trees"
	self sendLiteralTree: lTree distanceTree: dTree using: blTree bitLengths: bits.
	self sendCompressedBlock: lTree with: dTree.
%
category: 'fixed blocks'
method: ZipWriteStream
sendFixedBlock
	"Send a block using fixed huffman trees"
	self sendCompressedBlock: FixedLiteralTree with: FixedDistanceTree.
%
category: 'dynamic blocks'
method: ZipWriteStream
sendLiteralTree: lTree distanceTree: dTree using: blTree bitLengths: bits
	"Send all the trees needed for dynamic huffman tree encoding"
	| lastValue lastCount value |
	encoder nextBits: 5 put: (lTree maxCode - 256).
	encoder nextBits: 5 put: (dTree maxCode).
	self sendBitLengthTree: blTree.
	bits size = 0 ifTrue:[^self].
	lastValue := bits at: 1.
	lastCount := 1.
	2 to: bits size do:[:i|
		value := bits at: i.
		value = lastValue 
			ifTrue:[lastCount := lastCount + 1]
			ifFalse:[self sendBitLength: lastValue repeatCount: lastCount tree: blTree.
					lastValue := value.
					lastCount := 1]].
	self sendBitLength: lastValue repeatCount: lastCount tree: blTree.
%
category: 'stored blocks'
method: ZipWriteStream
sendStoredBlock
	"Send an uncompressed block"
	| inBytes |
	inBytes := blockPosition - blockStart.
	encoder flushBits. "Skip to byte boundary"
	encoder nextBits: 16 put: inBytes.
	encoder nextBits: 16 put: (inBytes bitXor: 16rFFFF).
	encoder flushBits.
	1 to: inBytes do:[:i|
		encoder nextBytePut: (itsCollection byteAt: blockStart+i)].
%
category: 'encoding'
method: ZipWriteStream
shouldFlush
	"Check if we should flush the current block.
	Flushing can be useful if the input characteristics change."
	| nLits |
	litCount = literals size ifTrue:[^true]. "We *must* flush"
	(litCount bitAnd: 16rFFF) = 0 ifFalse:[^false]. "Only check every N kbytes"
	matchCount * 10 <= litCount ifTrue:[
		"This is basically random data. 
		There is no need to flush early since the overhead
		for encoding the trees will add to the overall size"
		^false].
	"Try to adapt to the input data.
	We flush if the ratio between matches and literals
	changes beyound a certain threshold"
	nLits := litCount - matchCount.
	nLits <= matchCount ifTrue:[^false]. "whow! so many matches"
	^nLits * 4 <= matchCount
%
category: 'stored blocks'
method: ZipWriteStream
storedBlockSize
	"Compute the length for the current block when stored as is"
	^3 "block type bits" 
		+ (8 - (encoder bitPosition + 3 bitAnd: 7) bitAnd: 7)"skipped bits to byte boundary"
			+ 32 "byte length + chksum" 
				+ (blockPosition - blockStart * 8) "actual data bits".
%
category: 'private'
method: ZipWriteStream
updateCrc
	crcPosition < position ifTrue:[
		bytesWritten := bytesWritten + position - crcPosition + 1.
		crc := self updateCrc: crc from: crcPosition to: position - 1 in: itsCollection.
		crcPosition := position.
	].
%
category: 'private'
method: ZipWriteStream
updateCrc: oldCrc from: start to: stop in: aCollection
	^self class updateCrc: oldCrc from: start to: stop in: aCollection
%
category: 'deflating'
method: ZipWriteStream
updateHash: nextValue
	"Update the running hash value based on the next input byte.
	Return the new updated hash value."
	^((hashValue bitShift: HashShift) bitXor: nextValue) bitAnd: HashMask.
%
category: 'deflating'
method: ZipWriteStream
updateHashAt: here
	"Update the hash value at position here (one based)"
	^self updateHash: (itsCollection byteAt: here)
%
category: 'private'
method: ZipWriteStream
updateHashTable: table delta: delta
	| pos |
	1 to: table size do:[:i|
		"Discard entries that are out of range"
		(pos := table at: i) >= delta
			ifTrue:[table at: i put: pos - delta]
			ifFalse:[table at: i put: 0]].
%
category: 'deflating'
method: ZipWriteStream
validateMatchAt: pos from: startPos to: endPos
	| here |
	here := pos.
	startPos+1 to: endPos+1 do:[:i|
		(itsCollection at: i) = (itsCollection at: (here := here + 1))
			ifFalse:[^self error:'Not a match']].
	^true
%
category: 'initialize-release'
method: ZipWriteStream
writeFooter
	"Write footer information if necessary"
	crc := crc bitXor: 16rFFFFFFFF.
%
category: 'initialize-release'
method: ZipWriteStream
writeHeader
	"Write header information if necessary"
%
category: 'initialize-release'
method: ZipWriteStream
_initStreamWith: aCollectionOrStream
	crcPosition := 1.
	bytesWritten := 0.
	encoder := ZipEncoder on: aCollectionOrStream.
	self initialize.
	encoder isBinary
		ifTrue:[super _initStreamWith: (ByteArray new: WindowSize * 2)]
		ifFalse:[super _initStreamWith: (String new: WindowSize * 2)].
	crc := 16rFFFFFFFF.
	writeLimit := itsCollection size.
	self writeHeader.
%

! Remove existing behavior from SystemOrganizer
removeallmethods SystemOrganizer
removeallclassmethods SystemOrganizer
! ------------------- Class methods for SystemOrganizer
category: 'class initialization'
classmethod: SystemOrganizer
default 
	^ Default
%
category: 'instance creation'
classmethod: SystemOrganizer
defaultList: aSortedCollection

	^self new setDefaultList: aSortedCollection
%
category: 'class initialization'
classmethod: SystemOrganizer
initialize
	"	self  initialize	"
	
	Default _ 'as yet unclassified' asSymbol.
%
category: 'class initialization'
classmethod: SystemOrganizer
resetSystemOrganization
	"self resetSystemOrganization"

	| ar |
	ar := System myUserProfile symbolList dictionaryAndSymbolOf: self.
	(ar at: 1) at: #SystemOrganization put: self new.
%
! ------------------- Instance methods for SystemOrganizer
category: 'categories'
method: SystemOrganizer
addCategory: newCategory

	self categoryDict at: newCategory asSymbol put: Array new.
%
category: 'categories'
method: SystemOrganizer
addCategory: catString before: ignored
	"Add a new category named catString.
	Ignore before: arg until we start keeping an ordered list of categories"

	self addCategory: catString
%
category: 'accessing'
method: SystemOrganizer
categories
	"Answer an Array of categories (names)."

	^self categoryDict keys asArray sortAscending
%
category: 'private'
method: SystemOrganizer
categoryDict

	categoryDict == nil ifTrue: [ categoryDict := ClassOrganizer cachedOrganizer categories ].
	^categoryDict
%
category: 'accessing'
method: SystemOrganizer
categoryOfElement: element 
	"Answer the category associated with the argument, element."

	^self categoryDict keyAtValue: element ifAbsent: [ nil ]
%
category: 'accessing'
method: SystemOrganizer
classify: element under: heading
	"Store the argument, element, in the category named heading."

	| realHeading catName ar |
	heading == nil
		ifTrue: [ realHeading := Default ]
		ifFalse: [ realHeading := heading asSymbol ].
	(catName := self categoryOfElement: element) = realHeading ifTrue: [ ^self ].
	catName ~~ nil
		ifTrue: [
			realHeading = Default ifTrue: [ ^self ].
			self removeElement: element
		].
	ar := self categoryDict at: realHeading ifAbsent: [ nil ].
	ar == nil 
		ifTrue: [
			ar := Array new.
			self categoryDict at: realHeading put: ar.
		].
	ar add: element.
%
category: 'private'
method: SystemOrganizer
fileOutClasses: order on: stream

"Writes out code on the given stream that creates the given classes"
| class str lf head term any up |

order size == 0 ifTrue: [ ^self ].
head := 'doit
'.
term := '
' , '%
'.

lf := Character lf.
up := System myUserProfile.
1 to: order size do: [:j | | ar dict nm |
  class := order at: j.
  class fileOutPreClassOn: stream.
  ar := up dictionaryAndSymbolOf: class.
  dict := ar at: 1.
  nm := dict name.
  stream nextPutAll: head;
          nextPutAll:
    (class _modifiableDefinitionInDictionary: dict named: nm );
    nextPut: $.;
    nextPutAll: term
].

"now write out constraints and make non-modifiable classdict non-modifiable "
1 to: order size do: [:k | | ar dict |
  class := order at: k.
  any := false.
  ar := up dictionaryAndSymbolOf: class.
  dict := ar at: 1.
  str := class _constraintCreationExpressionIn: dict.
  str size > 0 ifTrue: [
    stream nextPutAll: head.
    stream nextPutAll: (dict keyAtValue: class); nextPut: $ ;
      nextPutAll: str;
      nextPut: $.; nextPut: lf.
    any := true.
  ].
  class isModifiable ifFalse: [
    any ifFalse: [
      stream nextPutAll: head.
      any := true
    ].
    stream nextPutAll: (dict keyAtValue: class); nextPutAll: ' immediateInvariant.'.
  ].
  any ifTrue: [
    stream nextPutAll: term
  ]
].
%
category: 'accessing'
method: SystemOrganizer
listAtCategoryNamed: cat

	^((self categoryDict at: cat asSymbol otherwise: #()) select: [:each |(System myUserProfile symbolList objectNamed: each name asString) ~~ nil]) collect: [:ea | ea name ]
%
category: 'accessing'
method: SystemOrganizer
listClassesAtCategoryNamed: cat
	^(self categoryDict at: cat asSymbol otherwise: [#()]) collect: [:each| each]
%
category: 'accessing'
method: SystemOrganizer
removeElement: element 

	| catName ar |
	catName := self categoryOfElement: element.
	catName == nil ifTrue: [ ^self ].
	ar := self categoryDict at: catName.
	ar remove: element ifAbsent: [].
%
category: 'accessing'
method: SystemOrganizer
removeEmptyCategories

	| removals |
	removals := Array new.
	self categoryDict keysAndValuesDo: [:catName :elements |
		elements isEmpty ifTrue: [ removals add: catName ].
	].
	removals do: [:catName | self removeSystemCategory: catName ].
%
category: 'categories'
method: SystemOrganizer
removeSystemCategory: catName

	self categoryDict removeKey: catName asSymbol ifAbsent: []
%
category: 'accessing'
method: SystemOrganizer
renameCategory: oldCatString toBe: newCatString
	"Rename a category. No action if new name already exists, or if old name does not exist."

	| oldCat newCat ar |
	oldCat := oldCatString asSymbol.
	newCat := newCatString asSymbol.
	(self categoryDict at: newCat ifAbsent: []) ~~ nil ifTrue: [ ^self ].
	(ar := self categoryDict at: oldCat ifAbsent: []) == nil ifTrue: [ ^self ].
	self categoryDict removeKey: oldCat.
	self categoryDict at: newCat put: ar
%
category: 'private'
method: SystemOrganizer
setDefaultList: aSortedCollection

	categoryDict _ Dictionary new.
	aSortedCollection do: [:each | self addCategory: each ]
%

run
ZipFileConstants initialize.
ZipConstants initialize.
ZipWriteStream initialize.
ZipReadStream initialize.
DataStream initialize.
Scanner initialize.
(System myUserProfile symbolList objectNamed: BootstrapSymbolDictionaryName)
    at: #SystemOrganization put: SystemOrganizer new.
MczInstaller initialize.
true
%
