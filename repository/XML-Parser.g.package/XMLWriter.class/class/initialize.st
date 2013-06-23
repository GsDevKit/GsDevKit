class initialization
initialize
	"XMLWriter initialize"

	XMLTranslation := Dictionary new.
	XMLTranslation
		at: Character cr put: '&#13;';
		at: Character lf put: '&#10;';
		at: Character tab put: '&#9;';
		at: $& put: '&amp;';
		at: $< put: '&lt;';
		at: $> put: '&gt;';
"		at: $' put: '&apos;'; "
		at: $" put: '&quot;'.
	XMLTranslationMap := ByteArray new: 256.
	XMLTranslation keysDo:[:ch| XMLTranslationMap at: ch asciiValue+1 put: 1].