content
documentAttributes: attributeList
	self document version: (attributeList at: 'version' ifAbsent: [nil]).
	self document encoding: (attributeList at: 'encoding' ifAbsent: [nil]).
	self document requiredMarkup: (attributeList at: 'requiredMarkup' ifAbsent: [nil]).