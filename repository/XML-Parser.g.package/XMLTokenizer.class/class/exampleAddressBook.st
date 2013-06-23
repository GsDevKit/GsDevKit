examples
exampleAddressBook
	| tokenizer |
	"XMLTokenizer exampleAddressBook"

	tokenizer _ XMLTokenizer on: self addressBookXML readStream.
	[tokenizer next notNil]
		whileTrue: []