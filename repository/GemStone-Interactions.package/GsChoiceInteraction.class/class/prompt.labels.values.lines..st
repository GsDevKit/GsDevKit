instance creation
prompt: aString labels: labelArray values: valueArray lines: lineArray
  ^ (self new
    prompt: aString;
    labels: labelArray;
    values: valueArray;
    lines: lineArray;
    yourself) signal