*monticello
default

  Smalltalk 
    at: #MCPlatformSupport
    ifPresent: [:cl |
	| d |
	d := cl transientGlobalForKey: #UUID_DEFUALT.
	d == nil 
		ifTrue: [
		 	d := self new initialize.
			cl transientGlobalForKey: #UUID_DEFUALT value: d.
		].
	^d ]
