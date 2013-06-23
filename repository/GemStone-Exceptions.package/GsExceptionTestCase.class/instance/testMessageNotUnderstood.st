tests
testMessageNotUnderstood
	"tests for Issue 161"
	| caughtIt exception args |

	caughtIt := false.
	[ 1 foo ] on: MessageNotUnderstood do: [:ex | 
		exception := ex.
		caughtIt := true ]. 
	self assert: caughtIt.
	self validateMNU: exception receiver: 1 selector: #foo messageArgs: #().

	caughtIt := false.
	[ 1 perform: #foo ] on: MessageNotUnderstood do: [:ex | 
		exception := ex.
		caughtIt := true ]. 
	self assert: caughtIt.
	self validateMNU: exception receiver: 1 selector: #foo messageArgs: #().

	caughtIt := false.
	[ 1 foo: 2 ] on: MessageNotUnderstood do: [:ex | 
		exception := ex.
		caughtIt := true ]. 
	self assert: caughtIt.
	self validateMNU: exception receiver: 1 selector: #foo: messageArgs: { 2 }.

	caughtIt := false.
	[ 1 perform: #foo: with: 2 ] on: MessageNotUnderstood do: [:ex | 
		exception := ex.
		caughtIt := true ]. 
	self assert: caughtIt.
	self validateMNU: exception receiver: 1 selector: #foo: messageArgs: { 2 }.

	caughtIt := false.
	[ MessageNotUnderstood signal ] on: MessageNotUnderstood do: [:ex | 
		exception := ex.
		caughtIt := true ]. 
	self assert: caughtIt.
	self validateMNU: exception receiver: nil selector: nil messageArgs: #().

	caughtIt := false.
	[ MessageNotUnderstood signal: 'message' ] on: MessageNotUnderstood do: [:ex | 
		exception := ex.
		caughtIt := true ]. 
	self assert: caughtIt.
	self validateMNU: exception receiver: nil selector: nil messageArgs: #().
