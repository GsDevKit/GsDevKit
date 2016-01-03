*change-notification
_defineClassNamed: aClassName with: aBlock
  | originalClass classOrganizer cls |
  originalClass := System myUserProfile symbolList
    objectNamed: aClassName asSymbol.
  originalClass ~~ nil
    ifTrue: [ 
      "pre-create the classOrganizer, so that original subclasses are preserved."
      classOrganizer := ClassOrganizer cachedOrganizer ].
  cls := aBlock value.
  cls == nil
    ifTrue: [ ^ nil ].
  originalClass ~~ nil
    ifTrue: [ 
      SystemChangeNotifier uniqueInstance
        doSilently: [ 
          | failedMethodCompilations |
          failedMethodCompilations := cls
            copyMCMethodsFrom: originalClass
            dictionaries: GsSession currentSession symbolList.
          failedMethodCompilations notEmpty
            ifTrue: [ 
              | errorStream |
              errorStream := WriteStream on: String new.
              errorStream
                nextPutAll:
                    'Not all methods for the class: ' , cls name printString
                        , ' were successfully recompled.';
                cr.
              failedMethodCompilations
                do: [ :ar | 
                  | details mth |
                  details := ar at: 3.
                  mth := ar at: 2.
                  errorStream
                    tab;
                    nextPutAll:
                        mth inClass name asString , '>>' , mth selector asString , ': ';
                    cr;
                    nextPutAll: details;
                    cr ].
              self error: errorStream contents ].
          MCPlatformSupport
            redefineSubclassesOf: originalClass
            using: classOrganizer.	"force redefinition of existing subclasses"
          MCPlatformSupport migrateInstancesWithSubclassesOf: cls ].
      cls category = originalClass category
        ifTrue: [ 
          SystemChangeNotifier uniqueInstance
            announce:
              (ClassModifiedAnnouncement itemClass: cls oldCategory: originalClass category) ]
        ifFalse: [ 
          SystemChangeNotifier uniqueInstance
            announce:
              (ClassMovedAnnouncement itemClass: cls oldCategory: originalClass category) ] ]
    ifFalse: [ 
      SystemChangeNotifier uniqueInstance
        announce: (ClassAddedAnnouncement itemClass: cls) ].
  SystemChangeNotifier uniqueInstance
    doSilently: [ Class _resolveUndeclaredSymbolsForClass: cls ].
  ^ cls