*bootstrap-instance creation
newForCategories

"Creates and returns a new instance of ClassOrganizer with a root of Object."

| inst |

inst := (super new) rootClass: Object.
inst updateClassInfoForCategories.
^inst