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
          MCPlatformSupport
            redefineSubclassesOf: originalClass
            using: classOrganizer.	"force redefinition of existing subclasses"
          MCPlatformSupport migrateInstancesWithSubclassesOf: cls.
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
                  | details def |
                  details := ar at: 1.
                  def := ar at: 2.
                  errorStream
                    tab;
                    nextPutAll: def description , ': ';
                    cr;
                    nextPutAll: details;
                    cr ]	"self error: errorStream contents" ] ].
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