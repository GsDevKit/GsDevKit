examples
exampleAddressBookWithDTD
	| tokenizer |
	"XMLTokenizer exampleAddressBookWithDTD"

	tokenizer _ XMLTokenizer on: self addressBookXMLWithDTD readStream.
	[tokenizer next notNil]
		whileTrue: []