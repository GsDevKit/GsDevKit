actions
removeClassHistory
	"remove old classes from the class history"
	self classes do: [:aClass | aClass removeClassHistory]