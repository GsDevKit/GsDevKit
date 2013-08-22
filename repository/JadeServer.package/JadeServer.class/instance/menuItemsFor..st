OmniBrowser
menuItemsFor: aMenu

	| items stream  |
	items := aMenu items.
	stream := WriteStream on: String new.
	items do: [:item |
		item isMenuLine 
			ifFalse: [
				stream 
					nextPutAll: item label; tab;
					nextPutAll: item target asOop printString; tab;
					nextPutAll: item selector asString; tab;
					nextPutAll: item enabled printString; tab;
					nextPutAll: item icon asString.
			].
		stream cr.
	].
	^stream contents