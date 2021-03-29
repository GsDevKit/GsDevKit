*bootstrap
updateClassInfoForCategories

"Causes the receiver to rescan for classes and rebuild internal structures. Skip rebuildHierarchy and collectClassNames, because we don't want the superclasses that are not in the users SymbolLists and don't care about AutoComplete"

self collectClasses; "find all visible classes"
    "rebuildHierarchy;" "build the hierarchy, fleshing out class set too"
    "collectClassNames;" "pull out the names of the classes and form auto-complete set"
    rebuildCategories "build the class categories"