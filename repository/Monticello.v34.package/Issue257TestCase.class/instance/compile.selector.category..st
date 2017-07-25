utilities
compile: aString selector: aSelector category: category
	"on 3.0 CompileError thrown ... no need for looking at result"
	self class 
		compileMethod: (aSelector asString , String cr , aString)
		category: category
		using: GsSession currentSession symbolList.
	^self class compiledMethodAt: aSelector