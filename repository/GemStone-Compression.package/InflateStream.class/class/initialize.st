class initialization
initialize
  "self initialize"

  | low high |
  MaxBits := 16.
  StateNewBlock := 0.
  StateNoMoreData := 1.
  BlockProceedBit := 8.
  BlockTypes := #(#'processStoredBlock' #'processFixedBlock' #'processDynamicBlock' #'errorBadBlock' #'proceedStoredBlock' #'proceedFixedBlock' #'proceedDynamicBlock' #'errorBadBlock').
  FixedLitCodes := ((1 to: 144) collect: [ :i | 8 ])
    , ((145 to: 256) collect: [ :i | 9 ]) , ((257 to: 280) collect: [ :i | 7 ])
    , ((281 to: 288) collect: [ :i | 8 ]).	"Initialize fixed block values"
  FixedDistCodes := (1 to: 32) collect: [ :i | 5 ]	"Init literal/length map"