accessing
default
	| d  o |
	d := SessionTemps current.
	o := d at: #'OBRecentClasses_SOLE_INSTANCE' ifAbsent: [].
	o == nil 
		ifTrue: [ 
			o := self new.
			d at: #'OBRecentClasses_SOLE_INSTANCE' put: o].
	^o