*change-notification
_announceMethodMoved: aMethod oldCategory: catSym
   SystemChangeNotifier uniqueInstance
      announce: (MethodMovedAnnouncement itemClass: self method: aMethod oldCategory: catSym)
