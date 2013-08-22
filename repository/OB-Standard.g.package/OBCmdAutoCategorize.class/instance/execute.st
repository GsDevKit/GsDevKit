execution
execute

	"Categorize methods by looking in parent classes for a method category."
	| co organizer classes |
	organizer _ target organization.
	classes := organizer withAllSuperclasses.
	co := ClassOrganizer newWithRoot: organizer from: ClassOrganizer new.
	co collectClasses; "find all visible classes"
    		collectClassNames; "pull out the names of the classes and form auto-complete set"
    		rebuildCategories. "build the class categories"
	((co searchForCategory: ClassOrganizer default in: co classes)
		collect: [:meth | meth selector]) do: [:sel | | found |
		found := (classes collect: [ :cls | cls categoryOfSelector: sel])
			detect: [:ea | ea asString ~= ClassOrganizer default and: [ ea ~= nil]]
			ifNone: [].
		found ifNotNil: [target theClass classify: sel under: found]].
	target organization removeEmptyCategories.
	self doAutoCommit