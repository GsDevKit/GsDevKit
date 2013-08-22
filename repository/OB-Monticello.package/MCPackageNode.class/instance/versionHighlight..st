private
versionHighlight: aString

	| verName inherited loaded |
	inherited := repositoryNode inherited.
	inherited ifNil: [inherited := #()].
	loaded := repositoryNode loaded.
	verName := (aString copyUpToLast: $.) copyUpTo: $(.
	^Text
		string: aString
		attribute: (TextEmphasis new emphasisCode: (
			((loaded includes: verName) ifTrue: [ 4 "underlined" ]
				ifFalse: [ (inherited includes: verName)
					ifTrue: [ 0 ]
					ifFalse: [ 1 "bold" ] ])))