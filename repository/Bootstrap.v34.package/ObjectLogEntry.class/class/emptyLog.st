actions
emptyLog
	"expect the caller to abort, acquire lock, and commit if necessary"

    self objectQueue removeAll.
    ObjectLog := nil.