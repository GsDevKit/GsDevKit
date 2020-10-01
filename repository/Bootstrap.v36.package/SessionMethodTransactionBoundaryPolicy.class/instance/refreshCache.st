private-actions
refreshCache
	"cache the new values"
	self cachedSessionMethodChangeCount: sessionMethodChangeCount.
	self cachedClassChangeCount: classChangeCount