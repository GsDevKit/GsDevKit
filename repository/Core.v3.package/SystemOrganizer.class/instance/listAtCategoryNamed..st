accessing
listAtCategoryNamed: cat
  ^ ((self categoryDict at: cat asSymbol otherwise: #())
    select: [ :each | (GsSession currentSession symbolList objectNamed: each name asString) ~~ nil ])
    collect: [ :ea | ea name ]