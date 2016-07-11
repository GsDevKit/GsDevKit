as yet unclassified
snapshotNew
  | packageInfo definitions categories |
  packageInfo := self packageInfo.
  definitions := OrderedCollection new.
  categories := packageInfo systemCategories.
  categories isEmpty
    ifFalse: [ definitions add: (MCOrganizationDefinition categories: categories) ].
  packageInfo
    coreMethods: [ :ea | definitions add: ea asMethodDefinition ]
    extensionMethods: [ :ea | definitions add: ea asMethodDefinition ]
    overriddenMethods: [ :ea | 
      definitions
        add:
          (packageInfo changeRecordForOverriddenMethod: ea) asMethodDefinition ]
    classes: [ :ea | definitions addAll: ea classDefinitions ].
  ^ MCSnapshot fromDefinitions: definitions