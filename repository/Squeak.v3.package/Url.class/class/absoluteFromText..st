*squeak
absoluteFromText: aString
  "Url absoluteFromText: 'http://chaos.resnet.gatech.edu:8000/docs/java/index.html?A%20query%20#part'"

  "Url absoluteFromText: 'msw://chaos.resnet.gatech.edu:9000/testbook?top'"

  "Url absoluteFromText: 'telnet:chaos.resnet.gatech.edu'"

  "Url absoluteFromText: 'file:/etc/passwd'"

  | remainder scheme fragment ind newUrl |
  "trim surrounding whitespace"
  remainder _ aString withBlanksTrimmed.	"extract the fragment, if any"
  ind _ remainder indexOf: $#.
  ind > 0
    ifTrue: [ 
      fragment _ remainder copyFrom: ind + 1 to: remainder size.
      remainder _ remainder copyFrom: 1 to: ind - 1 ].	"choose class based on the scheme name, and let that class do the bulk of the parsing"
  scheme _ self schemeNameForString: remainder.
  scheme = nil
    ifTrue: [ newUrl _ HttpUrl new privateInitializeFromText: remainder ].
  scheme = 'http'
    ifTrue: [ newUrl _ HttpUrl new privateInitializeFromText: remainder ].
  scheme = 'msw'
    ifTrue: [ newUrl _ MswUrl new privateInitializeFromText: remainder ].
  scheme = 'ftp'
    ifTrue: [ newUrl _ FtpUrl new privateInitializeFromText: remainder ].
  scheme = 'file'
    ifTrue: [ newUrl _ FileUrl new privateInitializeFromText: remainder ].
  scheme = 'browser'
    ifTrue: [ newUrl _ BrowserUrl new privateInitializeFromText: remainder ].
  scheme = 'mailto'
    ifTrue: [ newUrl _ MailtoUrl new privateInitializeFromText: remainder ].
  newUrl
    ifNil: [ 
      newUrl _ (self urlClassForScheme: scheme) new
        privateInitializeFromText: remainder ].	"set the fragment"
  newUrl privateFragment: fragment.	"all done"
  ^ newUrl