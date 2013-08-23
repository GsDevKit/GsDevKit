actions
getAnnotations
	^ String streamContents: 
		[ :aStream | 
		((self annotationRequests collect: [ :request | self perform: request ]) select: [ :stringOrNil | stringOrNil isEmptyOrNil not ]) 
			do: [ :ea | aStream nextPutAll: ea ]
			separatedBy: [ aStream nextPutAll: separator ] ]