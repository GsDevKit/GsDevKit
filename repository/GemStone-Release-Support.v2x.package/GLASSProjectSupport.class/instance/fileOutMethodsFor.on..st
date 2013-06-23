bootstrap fileout
fileOutMethodsFor: cl on: stream

	| lf sels sel clamps cat nm |

	nm := cl nameForFileout.
	lf := Character lf.
	stream nextPut: lf.

	stream 
		nextPutAll: '! Remove existing behavior from '; nextPutAll: nm; nextPut: lf;
		nextPutAll: 'removeallmethods ', nm; nextPut: lf;
		nextPutAll: 'removeallclassmethods ', nm; nextPut: lf.

	stream nextPutAll: '! ------------------- Class methods for '; nextPutAll: nm; nextPut: lf.
	sels := SortedCollection withAll: (cl class selectors).
	1 to: sels size do: [:i |
  		sel := sels at: i.
		((cl class _categoryOfSelector: sel) beginsWith: '*')
			ifFalse: [
				
  				stream nextPutAll: 'category: '''; nextPutAll: (cl class _categoryOfSelector: sel);
      				nextPut: $'; nextPut: lf.
  				cl class fileOutMethod: sel on: stream ]].

	stream nextPutAll: '! ------------------- Instance methods for '; nextPutAll: nm; nextPut: lf.
	sels := SortedCollection withAll: (cl selectors).
	1 to: sels size do: [:i |
  		sel := sels at: i.
		((cl _categoryOfSelector: sel) beginsWith: '*')
			ifFalse: [
  				stream nextPutAll: 'category: '''; nextPutAll: (cl _categoryOfSelector: sel);
      				nextPut: $'; nextPut: lf.
  				cl fileOutMethod: sel on: stream ]].

	cl fileOutPostMethodsOn: stream.
