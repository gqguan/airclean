      INTEGER FUNCTION glcavar(NVARS,ICOL,IROW)
      PARAMETER (IPARAM_NVARS=30)
      INTEGER IVARIABLES(8,IPARAM_NVARS), IVRSN
      DATA IVARIABLES/
     1  4HOPT0,  4H    ,  4H    ,  4H    ,  0,  -1,  0,  1,
     1  4HOPT1,  4H    ,  4H    ,  4H    ,  0,  -1,  0,  1,
     1  4HOPT2,  4H    ,  4H    ,  4H    ,  0,  -1,  0,  1,
     1  4HOPT3,  4H    ,  4H    ,  4H    ,  0,  -1,  0,  1,
     1  4HOPT4,  4H    ,  4H    ,  4H    ,  0,  -1,  0,  1,
     1  4HOPT5,  4H    ,  4H    ,  4H    ,  0,  -1,  0,  3,
     1  4HOPT6,  4H    ,  4H    ,  4H    ,  0,  -1,  0,  3,
     1  4HOPT7,  4H    ,  4H    ,  4H    ,  0,  -1,  0,  3,
     1  4HOPT8,  4H    ,  4H    ,  4H    ,  0,  -1,  0,  3,
     1  4HOPT9,  4H    ,  4H    ,  4H    ,  0,  -1,  0,  3,
     1  4HDHOL,  4HE   ,  4H    ,  4H    ,  1,  -1,  0,  1,
     1  4HPITC,  4HH   ,  4H    ,  4H    ,  1,  -1,  0,  1,
     1  4HM0  ,  4H    ,  4H    ,  4H    ,  1,  -1,  0,  1,
     1  4HM1  ,  4H    ,  4H    ,  4H    ,  1,  -1,  0,  1,
     1  4HM2  ,  4H    ,  4H    ,  4H    ,  1,  -1,  0,  1,
     1  4HM3  ,  4H    ,  4H    ,  4H    ,  1,  -1,  0,  1,
     1  4HM4  ,  4H    ,  4H    ,  4H    ,  1,  -1,  0,  1,
     1  4HM5  ,  4H    ,  4H    ,  4H    ,  1,  -1,  0,  1,
     1  4HM6  ,  4H    ,  4H    ,  4H    ,  1,  -1,  0,  1,
     1  4HM7  ,  4H    ,  4H    ,  4H    ,  1,  -1,  0,  1,
     1  4HRFU0,  4H    ,  4H    ,  4H    ,  1,  -1,  0,  3,
     1  4HDP  ,  4H    ,  4H    ,  4H    ,  1,  -1,  0,  3,
     1  4HS   ,  4H    ,  4H    ,  4H    ,  1,  -1,  0,  2,
     1  4HT   ,  4H    ,  4H    ,  4H    ,  1,  -1,  0,  2,
     1  4HPHI ,  4H    ,  4H    ,  4H    ,  1,  -1,  0,  2,
     1  4HRE_G,  4H    ,  4H    ,  4H    ,  1,  -1,  0,  2,
     1  4HRE_L,  4H    ,  4H    ,  4H    ,  1,  -1,  0,  2,
     1  4HETA ,  4H    ,  4H    ,  4H    ,  1,  -1,  0,  2,
     1  4HRFU1,  4H    ,  4H    ,  4H    ,  1,  -1,  0,  2,
     1  4HRFU2,  4H    ,  4H    ,  4H    ,  1,  -1,  0,  2/

      DATA IVRSN/1470411632/
      NVARS = IPARAM_NVARS
      IF(IROW .EQ. 0) THEN
          glcavar=IVRSN
      ELSE IF(IROW .LE. NVARS) THEN
          glcavar=IVARIABLES(ICOL,IROW)
      ENDIF
      RETURN
      END
