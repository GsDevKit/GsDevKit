instance creation
prompt: prompt confirm: confirm cancel: cancel abort: abort
  "on confirm return true, on cancel return false on abort return nil"

  ^ self new
    prompt: prompt;
    confirm: confirm;
    cancel: cancel;
    abort: abort