*ob-tools
_stepOverInFrame: level breakpointOnStack: breakpointOnStack mode: raiseException replace: replaceTos tos: tos
    "Step execution so that it stops after the next message send
     in the specified stack level.  If a return causes the
     context at the specified level to be removed from the
     stack, execution will stop immediately after that return.
     If 'raiseException' then debugger exceptions are raised
     so that a GCI app can catch them.
     Returns the receiver unless the receiver completes in which
     case the result of the completion is returned."

  | actualLevel result saveArray breakpointsToIgnore 
    mySaveArray saveStackDepth status |

  self _checkIfDebuggable.
  status := self _statusString.
  (status = 'ready') ifTrue: [
    self _unscheduleProcess.
  ].

  actualLevel := level.
  (actualLevel < 0 _or:[ actualLevel > stackDepth ]) ifTrue:[
    level _error: #rtErrArgOutOfRange .
  ].

  saveStackDepth := stackDepth.
  (actualLevel == 0) ifTrue: [
    saveArray := self _setBreaksForStepInto.
    breakpointsToIgnore := (saveArray at: 3).
    "set INT_STEP_INTO_FROM_TOS_MASK bit"
    interruptFlag := interruptFlag bitOr: 16r200.
  ] ifFalse: [
    saveArray := self _localSetBreaksForStepOverFromLevel: actualLevel breakpointOnStack: breakpointOnStack.
    breakpointsToIgnore := (saveArray at: 3).
    (level == 1) ifTrue: [
      "step over from TOS, so set the interrupt bit that will cause
       a stack break to be inserted after the next send, to trap return
       from that send."
      "Set INT_STEP_OVER_FROM_TOS_MASK bit"
      interruptFlag := interruptFlag bitOr: 16r100.
    ].
  ].

  (replaceTos) ifTrue: [
    mySaveArray := #[ (saveArray at: 1), (saveArray at: 2), saveStackDepth,
                       tos].
  ] ifFalse: [
    mySaveArray := #[ (saveArray at: 1), (saveArray at: 2), saveStackDepth].
  ].

  _debugMode := true.
  "Make sure the activeProcess exists"
  result := self _primStep: breakpointsToIgnore
                 from: (ProcessorScheduler scheduler activeProcess)
                 with: mySaveArray mode: raiseException.

  (status = 'ready') ifTrue: [
    (result == self) ifTrue: [
      (self _isTerminated) ifFalse: [
        ProcessorScheduler scheduler _scheduleProcess: self.
      ].
    ].
  ].
  "_stepCleanup may run a method that was stepped into (Object>>at:), so run with
   #_performNoDebug:with: to ensure that the cleanup is performed"
  self _performNoDebug: #_stepCleanup: with: mySaveArray.
  ^result