opening
openOnClass: aClass selector: aSelector
       aSelector ifNil: [^ (self onClass: aClass) open].
       ^ (self onClass: aClass selector: aSelector) open.