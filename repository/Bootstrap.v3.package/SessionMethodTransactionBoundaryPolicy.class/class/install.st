installation
install

	super install.
	"Make sure new instance writable by everyone"
	Current changeToSegment: nil.