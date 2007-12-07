--{{{  BitsPerWord etc
--                                                                 16 bit   32 bit
VAL BytesPerWord   IS ##TARGET.BYTES.PER.WORD                 : --  2            4
VAL BitsPerWord    IS ##TARGET.BITS.PER.WORD                  : -- 16           32
VAL SignBit        IS MOSTNEG INT                             : -- #8000 #80000000
VAL MaxUnsignedInt IS BITNOT 0                                : -- #FFFF #FFFFFFFF
VAL MaxInt         IS MaxUnsignedInt                          : -- #FFFF #FFFFFFFF
--}}}

--{{{  Truncate or Nearest
VAL Truncate IS 0 :                --op code for round to zero
VAL Nearest  IS 1 :                --op code for round to nearest
--}}}
--{{{  Rounding Modes
VAL RZ IS 0 :
VAL RN IS 1 :
VAL RP IS 2 :
VAL RM IS 3 :
--}}}
--{{{  VALs for Op of REALxxOP and IEEExxOP
VAL add IS 0 :
VAL sub IS 1 :
VAL mul IS 2 :
VAL div IS 3 :

VAL Add IS add :
VAL Sub IS sub :
VAL Mul IS mul :
VAL Div IS div :
--}}}

--{{{  IEEE Single length constants
VAL RealShift  IS         8 :
VAL BitsInFrac IS        24 :
VAL RealExp    IS       255 :
VAL RealXcess  IS       127 :
VAL RealRBit   IS       128 :
#IF (TARGET.BITS.PER.WORD = 32)
VAL RealInf    IS #7F800000 :
#ENDIF
--}}}
--{{{  IEEE Double length constants
VAL DRealShift  IS   11 :
VAL DBitsInFrac IS   53 :
VAL DRealExpSh  IS   20 :
VAL DRealExp    IS #7FF :
VAL DRealXcess  IS 1023 :
VAL DRealRBit   IS #400 :
#IF (TARGET.BITS.PER.WORD = 32)
VAL DRealInf    IS #7FF00000 :
#ENDIF
--}}}
