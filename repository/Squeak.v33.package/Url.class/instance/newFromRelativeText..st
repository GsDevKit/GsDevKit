parsing
newFromRelativeText: aString
	"return a URL relative to the current one, given by aString.  For instance, if self is 'http://host/dir/file', and aString is '/dir2/file2', then the return will be a Url for 'http://host/dir2/file2'"

	"if the scheme is the same, or not specified, then use the same class"

	| newSchemeName remainder fragmentStart newFragment newUrl bare |

	bare _ aString withBlanksTrimmed.
	newSchemeName _ Url schemeNameForString: bare.
	(newSchemeName isNil not and: [ newSchemeName ~= self schemeName ]) ifTrue: [
		"different scheme -- start from scratch"
		^Url absoluteFromText: aString ].

	remainder _ bare.

	"remove the fragment, if any"
	fragmentStart _ remainder indexOf: $#.
	fragmentStart > 0 ifTrue: [
		newFragment _ remainder copyFrom: fragmentStart+1 to: remainder size. 
		remainder _ remainder copyFrom: 1 to: fragmentStart-1].

	"remove the scheme name"
	newSchemeName ifNotNil: [
		remainder _ remainder copyFrom: (newSchemeName size + 2) to: remainder size ].

	"create and initialize the new url"
	newUrl _ self class new privateInitializeFromText: remainder  relativeTo: self.


	"set the fragment"
	newUrl privateFragment: newFragment.


	^newUrl