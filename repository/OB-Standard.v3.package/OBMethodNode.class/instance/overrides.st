navigating
overrides
	| classes |
	classes := OrderedCollection new.
	self addOverridersOf: self selector inClass: self theClass to: classes.
	^ classes collect: [:ea | OBMethodNode on: selector inClass: ea]