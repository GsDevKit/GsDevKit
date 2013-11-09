accessing
category
  | sel cat |
  (cat := category) ifNil:[
    (sel := self selector) ifNotNil:[
       cat := self theClass whichCategoryIncludesSelector: sel .
    ].
    cat ifNil:[ cat := ClassOrganizer default ].
    category := cat .
  ].
  ^ cat
