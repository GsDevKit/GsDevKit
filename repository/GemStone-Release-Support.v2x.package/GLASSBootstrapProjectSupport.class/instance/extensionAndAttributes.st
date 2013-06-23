driver
extensionAndAttributes
	"For creating bootstrap artifacts, produce  the attributes for2.4 and 2.3 together"
	
	^(self bootstrapFileExtension = '2'
		ifTrue: [ 
			{
				{'23'. #( #gemstone #'gs2.x' #'gs2.3.x'  ). }.
				{'24'. #(  #gemstone #'gs2.x' #'gs2.4.x' ). }. 
			}  ]
		ifFalse: [ 
			{
				{'30'. #( #gemstone #'gs3.x' ). }. 
			}  ])
