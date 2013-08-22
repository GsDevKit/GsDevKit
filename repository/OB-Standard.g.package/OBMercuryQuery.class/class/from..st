instance creation
from: aString 
	| rawQuery |
	rawQuery := aString asString withBlanksTrimmed.
	rawQuery first = $' ifTrue: [^ OBMStringQuery new setRawQuery: rawQuery].
	^ (self selectClassForQuery: rawQuery) new setRawQuery: rawQuery