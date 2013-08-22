callbacks
accept: aText notifying: anObject 
 	aText asString withBlanksTrimmed ifEmpty: [^self].
	OBMercuryQuery find: aText for: self.
	self changed: #text.
	^nil