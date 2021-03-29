open/close
reopen
	"Close and reopen this file. The file position is reset to zero."

	self close.
	mode := 'r+b'.
	self open: pathName mode: mode