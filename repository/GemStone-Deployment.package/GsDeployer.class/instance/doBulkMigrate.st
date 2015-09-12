migration
doBulkMigrate
  | classList instanceLists instanceCount array |
  classList := self classesWithClassHistory.
  self bulkMigrate
    ifTrue: [ 
      Transcript
        cr;
        show:
            'Bulk migrate of ' , classList size printString , ' candidate classes ' ].
  instanceLists := (self objectNamed: #'UserGlobals')
    at: #'GS_BULK_MIGRATE_INSTANCE_LISTS'
    put: OrderedCollection new.
  self commit.
  [ 
  instanceCount := 0.
  array := Array new: 1.
  1 to: classList size by: 2000 do: [ :index | 
    | limitList instancesList |
    "listInstances limited to array of 2034 classes in GemStone3.0"
    limitList := classList
      copyFrom: index
      to: (index + 1999 min: classList size).
    self commit.
    instancesList := SystemRepository listInstances: limitList.
    instanceLists add: instancesList.
    self commit ].
  instanceLists
    do: [ :instancesList | 
      instancesList
        do: [ :instances | 
          instanceCount := instanceCount + instances size.
          instances
            do: [ :instance | 
              array at: 1 put: instance.
              instance class
                migrateInstances: array
                to: instance class classHistory last ] ] ].
  classList size = 0
    ifTrue: [ 
      Transcript
        cr;
        tab;
        show: 'No instance migrations performed.' ]
    ifFalse: [ 
      Transcript
        cr;
        tab;
        show:
            'Migrated ' , instanceCount printString , ' instances for '
                , classList size printString , ' classes.' ].
  classList
    do: [ :each | 
      | history |
      history := each classHistory.
      [ 1 < history size ] whileTrue: [ history first removeFromClassHistory ] ] ]
    ensure: [ 
      (self objectNamed: #'UserGlobals')
        removeKey: #'GS_BULK_MIGRATE_INSTANCE_LISTS'
        ifAbsent: [  ].
      self commit ]