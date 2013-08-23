*ob-standard
selectorsContaining: aString
	"Answer a list of selectors that contain aString within them. Case-insensitive.  Does return symbols that begin with a capital letter."

	| size selectorList ascii |

	selectorList _ OrderedCollection new.
	(size _ aString size) = 0 ifTrue: [^selectorList].

	SystemNavigation default allSelectors do: [:each |
		each size >= size ifTrue:
			[(each includesString: aString)
						ifTrue: [selectorList add: each]]].

	^selectorList reject: [:each | "reject non-selectors"
		each numArgs < 0 ].

"Symbol selectorsContaining: 'scon'"