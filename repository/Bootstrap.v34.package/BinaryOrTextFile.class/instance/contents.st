Reading
contents

	^self isBinary
		ifTrue: [ super contents asByteArray ]
		ifFalse: [ super contents ]