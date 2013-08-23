*ob-tools
_localSetBreaksForStepOverFromLevel: aLevel breakpointOnStack: breakpointOnStack 

"Set breakpoints so that a subsequent IntContinue on the receiver will
 execute a step-over.  

 For use only from within implementation of GciStep.

 Return an Array, #[ the GsMethod in which single-step breaks were set;
		     the level at which stack break was set, or zero;
                     <number of breakpoints to ignore> ]."

"Algorithm

if (aLevel >= 3)
  set stack break at level aLevel .
else if ((aLevel <= 2) and 
         (next instruction to execute at level 1 is BC_CALL_PRIMITIVE))
  set all step breakpoints in level 2 and set stack breakpoint in level 3
else 
  set stack breakpoint at level 2  
"

| tosFrame tosMethod aMethod stackBreakLevel result breakpointsToIgnore |
result := Array new: 3 .
aLevel >= 3 ifTrue:[
  stackBreakLevel := aLevel .
  breakpointsToIgnore := breakpointOnStack ifTrue: [ 0 ] ifFalse: [ 1 ]]
ifFalse:[ "aLevel is 1 or 2"
  tosFrame := self _frameContentsAt: aLevel .
  tosMethod := tosFrame at: 1 "virtual machine constant" .
  (tosMethod _opcodeAtIsCallPrimitive: 
      (tosFrame at: 2 "virtual machine constant" "ipOffset")) ifTrue:[
    "next instruction to be executed is a call-primitive, so set breaks in
     method where send resolved to a primitive."
    aMethod := self methodAt: 2 .
    aMethod _setAllStepBreaks .
    breakpointsToIgnore := 0 .
    result at: 1 put: aMethod .
    stackBreakLevel := 3  .
    ]
  ifFalse:[
    "next instruction is not a primitive call, set breaks in TOS method"
    tosMethod _setAllStepBreaks .
    result at: 1 put: tosMethod .
    stackBreakLevel := 2.
    breakpointsToIgnore := 1 .
    ].
  ].
stackBreakLevel <= stackDepth 
  ifTrue:[ breakpointOnStack ifFalse: [ self _setStackBreakAt: stackBreakLevel ]]
  ifFalse:[ stackBreakLevel := 0 "no stack breaks to set" ].
result at: 3 put: breakpointsToIgnore .
result at: 2 put: stackBreakLevel .
^ result