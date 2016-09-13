*bootstrap-caching
cachedOrganizer

	| d  o |
	d := SessionTemps current.
	o := d at: #Cached_Class_Organizer ifAbsent: [].
	o == nil 
		ifTrue: [ 
			o := CachingClassOrganizer new.
			d at: #Cached_Class_Organizer put: o].
	^o