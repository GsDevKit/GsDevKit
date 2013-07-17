listing
foreignClasses
	| s |
	s := IdentitySet new.
	self foreignSystemCategories
		do: [:c | (SystemOrganizer new listAtCategoryNamed: c)
				do: [:cl | 
					| cls | 
					cls := Smalltalk classNamed: cl. 
					s add: cls;
					  add: cls class]].
	^ s