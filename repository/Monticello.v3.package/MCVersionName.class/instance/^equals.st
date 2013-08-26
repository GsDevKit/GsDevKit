comparing
= aString 
	"Ignore any file-extension for comparison of MCVersionNames."
	| myVersionName |
	aString isString ifFalse: [ ^ false ].
	myVersionName := self versionName.
	^ myVersionName = aString asMCVersionName versionName
