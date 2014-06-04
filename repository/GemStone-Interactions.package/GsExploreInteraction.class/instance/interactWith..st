interacting
interactWith: anObject
  "opportunity for double dispatch:

     interactWithChoice:
     interactWithConfirm:
     interactWithInform:
     interactWithInspect:
     interactWithMultiLineText:
     interactWithText:
  "

  ^ anObject interactWithExplore: self