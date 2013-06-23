testing
testExceptionalFloats
	"http://code.google.com/p/glassdb/issues/detail?id=294"

	| x expected |
	expected := #('PlusInfinity' 'MinusInfinity' 'PlusQuietNaN' 'PlusQuietNaN' 'PlusSignalingNaN' 'PlusSignalingNaN' 'PlusInfinity' 'MinusInfinity' 'PlusQuietNaN').
	self
		assert:
			(x := #('PlusInfinity' 'MinusInfinity' 'PlusQuietNaN' 'MinusQuietNaN' 'PlusSignalingNaN' 'MinusSignalingNaN' 'Infinity' '-Infinity' 'NaN' )
				collect: [ :str | str asNumber asString ])
				= expected