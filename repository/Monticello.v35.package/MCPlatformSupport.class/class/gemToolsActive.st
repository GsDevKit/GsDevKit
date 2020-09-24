smalltalk
gemToolsActive

   Smalltalk at: #OBGemStonePlatform
         ifPresent: [:obGsPlatClass | ^(obGsPlatClass perform: #clientForwarderMap) includesKey: #debugger].
   ^ false