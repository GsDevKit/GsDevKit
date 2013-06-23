Private
register

	|set |
	set := self transientGlobals at: #TransientRandom otherwise: nil.
	set == nil 
		ifTrue: [
			set := IdentitySet new.
			self transientGlobals at: #TransientRandom put: set
		].
	set add: self