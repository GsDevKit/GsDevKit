accessing-dynamic
sourceMap
	"Answer a mapping from byte codes to source code ranges."
	
	^ sourceMap ifNil: [ sourceMap := self context sourceMap ]