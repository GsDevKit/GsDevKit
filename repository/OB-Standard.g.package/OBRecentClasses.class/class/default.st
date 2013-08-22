accessing
default
	^ soleInstance ifNil: [soleInstance := self new]