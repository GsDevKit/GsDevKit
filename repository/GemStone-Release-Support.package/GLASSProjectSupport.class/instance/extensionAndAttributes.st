driver
extensionAndAttributes
	"For creating release artifacts, use the attributes for the specific system only"
	| gsVersion |
	gsVersion := System stoneVersionAt: 'gsVersion'.
	(gsVersion beginsWith: '2.3')
		ifTrue: [
			^{
				{'23'. #( #gemstone #'gs2.x' #'gs2.3.x'  ). }. 
			} ].
	(gsVersion beginsWith: '2.4')
		ifTrue: [
			^{
				{'24'. #(  #gemstone #'gs2.x' #'gs2.4.x' ). }. 
			} ].
	(gsVersion beginsWith: '3.0')
		ifTrue: [
			^{
				{'30'. #( #gemstone #'gs3.x' ). }. 
			} ].
	"Assume 3.1"
	^{
		{'30'. #( #gemstone #'gs3.x' #'gs3.1.x' ). }. 
	}  	
