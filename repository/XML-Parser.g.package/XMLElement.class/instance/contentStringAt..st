accessing
contentStringAt: entityName
	^(self elementAt: entityName ifAbsent: [^'']) contentString