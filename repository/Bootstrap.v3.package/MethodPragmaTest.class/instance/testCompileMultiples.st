testing-compiler
testCompileMultiples

	| body |
	body := '<required>
	<view: #passwordInput>
	'.
	self assertPragmas: body givesKeywords: #(required view:) arguments: #( () (passwordInput)).
	body := '<matches: ''[A-Za-z\s]+'' error: ''Only alpha characters and whitespace are allowed''>
	'.
	self assertPragmas: body givesKeywords: #(matches:error:) arguments: #( ('[A-Za-z\s]+' 'Only alpha characters and whitespace are allowed') ).
	body := '<label: ''Enter Body:''>
	<view: #textArea>
	'.
	self assertPragmas: body givesKeywords: #(label: view:) arguments: #( ('Enter Body:') (textArea)).