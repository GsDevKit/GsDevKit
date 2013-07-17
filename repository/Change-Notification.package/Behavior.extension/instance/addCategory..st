*change-notification
addCategory: aString

| aSymbol seg canWrite |
"Adds aString as a method category for the receiver.  If aString is already a
 method category, generates an error."

self _validatePrivilege.
aSymbol := aString asSymbol .
(self _includesCategory: aSymbol)
ifTrue:[ ^ self _error: #classErrMethCatExists
                  args: #[aString, self]]. "category already exists for class"
seg := self segment.
canWrite := (seg == nil
		ifTrue: [ true ]
		ifFalse: [ seg currentUserCanWrite ]).
canWrite
  ifTrue: [ categories add: (SymbolAssociation newWithKey: aSymbol value: SymbolSet new ) ]
  ifFalse: [ GsPackagePolicy current addCategory: aSymbol for: self ].
SystemChangeNotifier uniqueInstance 
	announce: (ClassReorganizedAnnouncement itemClass: self)