*change-notification
removeEmptyCategories
	"Remove empty categories."

	| emptyCats |
	emptyCats := Set new.
	self _categories associationsDo: [:assoc |
		assoc value isEmpty ifTrue: [ emptyCats add: assoc key ].
	].
	SystemChangeNotifier uniqueInstance doSilently: [
		emptyCats do: [:cat | self removeCategory: cat ]].
	SystemChangeNotifier uniqueInstance
		announce: (ClassReorganizedAnnouncement itemClass: self)
