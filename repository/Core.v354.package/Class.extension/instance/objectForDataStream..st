*core-squeak
objectForDataStream: refStrm
	| |
	"I am about to be written on an object file.  Write a reference to a class instead."

	^ DiskProxy global: self name selector: #withClassVersion:
				args: { self classVersion }