*squeak-bootstrap
platformSupport

    | platformSupport |
    platformSupport := GsSession currentSession symbolList objectNamed: #MCPlatformSupport.
	platformSupport ~~ nil ifTrue: [^platformSupport].

	platformSupport := GsSession currentSession symbolList objectNamed: #SeasidePlatformSupport.
	platformSupport ~~ nil ifTrue: [^platformSupport].

	platformSupport := GsSession currentSession symbolList objectNamed: #WAPlatform.
	platformSupport ~~ nil ifTrue: [^platformSupport current].

	^nil.
