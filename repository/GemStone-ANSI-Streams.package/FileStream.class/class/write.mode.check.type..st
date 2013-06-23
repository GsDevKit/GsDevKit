instance creation
write: path mode: mode check: checkBool type: type
	"mode: #create, #append, #truncate
	 check: 
	  mode == #create and check == false and the file exists, then the file is used
	  mode == #create and check == true and the file exists, an error is thrown
	  mode == #append and check == false and the file does not exist then it is created
	  mode == #append and check == true and the file does not exist an error is thrown
	  mode == #truncate and check == false and file does not exist then it is created
	  mode == #truncate and check == true and the file does not exist an error is thrown
	 type: #binary, #text, #serverBinary, #serverText, #clientBinary, #clientText
	"
	
	^(self basicNew) openForWrite: path mode: mode check: checkBool type: type