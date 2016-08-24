      INTEGER FUNCTION glcavar(NVARS,ICOL,IROW)
      PARAMETER (IPARAM_NVARS=14)
      INTEGER IVARIABLES(8,IPARAM_NVARS), IVRSN
      DATA IVARIABLES/
     1  4HOPT0,  4H    ,  4H    ,  4H    ,  0,  -1,  0,  1,
     1  4HOPT1,  4H    ,  4H    ,  4H    ,  0,  -1,  0,  1,
     1  4HOSIZ,  4HE   ,  4H    ,  4H    ,  1,  -1,  0,  1,
     1  4HPITC,  4HH   ,  4H    ,  4H    ,  1,  -1,  0,  1,
     1  4HCOEF,  4HF1  ,  4H    ,  4H    ,  1,  -1,  0,  1,
     1  4HCOEF,  4HF2  ,  4H    ,  4H    ,  1,  -1,  0,  1,
     1  4HCOEF,  4HF3  ,  4H    ,  4H    ,  1,  -1,  0,  1,
     1  4HCOEF,  4HF4  ,  4H    ,  4H    ,  1,  -1,  0,  1,
     1  4HCOEF,  4HF5  ,  4H    ,  4H    ,  1,  -1,  0,  1,
     1  4HCOEF,  4HF6  ,  4H    ,  4H    ,  1,  -1,  0,  1,
     1  4HCOEF,  4HF7  ,  4H    ,  4H    ,  1,  -1,  0,  1,
     1  4HCOEF,  4HF8  ,  4H    ,  4H    ,  1,  -1,  0,  1,
     1  4HSA  ,  4H    ,  4H    ,  4H    ,  1,  -1,  0,  2,
     1  4HETA ,  4H    ,  4H    ,  4H    ,  1,  -1,  0,  2/

      DATA IVRSN/1471643559/
      NVARS = IPARAM_NVARS
      IF(IROW .EQ. 0) THEN
          glcavar=IVRSN
      ELSE IF(IROW .LE. NVARS) THEN
          glcavar=IVARIABLES(ICOL,IROW)
      ENDIF
      RETURN
      END
