private
getCategories: labels reject: reject lines: lines 
	| cats |
	target theClass allSuperclassesDo: 
		[ : cls | 
		cls = Object ifFalse: 
			[ cats := cls organization categories reject: [ : cat | reject includes: cat ].
			cats isEmpty ifFalse: 
				[ lines add: labels size.
				labels addAll: cats asSortedCollection.
				reject addAll: cats ] ] ]