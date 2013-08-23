compiling
fileIn
	(self theClass) ifNotNilDo: [:class | 
		class
			compile: self source 
			classified: self category.
		class setStamp: self stamp forMethod: self selector]