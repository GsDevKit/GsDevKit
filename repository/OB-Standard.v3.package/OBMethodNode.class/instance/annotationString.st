displaying
annotationString
	^ (OBAnnotationRequest onClass: self theClass selector: self selector)
		getAnnotations.