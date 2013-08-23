instance creation
from: aString panel: aPanel 
	| rawQuery |
	rawQuery := aString asString withBlanksTrimmed.
	rawQuery first = $' ifTrue: [^ OBMStringQuery new setRawQuery: rawQuery].
	^ (self selectClassForQuery: rawQuery) new setRawQuery: rawQuery
		panel: aPanel