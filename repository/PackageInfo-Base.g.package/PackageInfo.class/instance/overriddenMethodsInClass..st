listing
overriddenMethodsInClass: aClass
	^ (self overrideCategoriesForClass: aClass) gather: [:ea | self methodsInCategory: ea ofClass: aClass]