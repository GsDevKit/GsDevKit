Testing
testRandom
       "Random testRandom will check your hardware environment"
|num|
num := Random new initialize.
num seed: 1.
10000 timesRepeat: [
  num next.
].
(1043618065 = num seed)
  ifTrue: [ ^'OK.' ]
  ifFalse: [ ^'Bad result. Fix this generator
             by referring to Park & Miller,^M
             Communications of the ACM 31:10:1192-1201, 1988.' ].