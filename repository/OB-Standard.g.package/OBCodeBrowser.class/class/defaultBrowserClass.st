configuration
defaultBrowserClass
    | browserOpener |
    Smalltalk at: #SystemBrowser ifPresent: [:class |
	    browserOpener := class default.
	    (browserOpener name beginsWith: 'OB') ifTrue: [^browserOpener browserClass].
    ].
    ^OBSystemBrowser