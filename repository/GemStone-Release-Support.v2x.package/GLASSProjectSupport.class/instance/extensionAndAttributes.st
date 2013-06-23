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
	"Assume 3.0"
	^{
		{'30'. #( #gemstone #'gs3.x' ). }. 
	}  	
