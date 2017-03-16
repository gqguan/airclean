C$ #1 BY: ANAVI DATE:  1-JUL-1994 NEW FOR USER MODELS
C
C     User Unit Operation Model (or Report) Subroutine for USER2
C     
      SUBROUTINE GLCA  (NMATI,  SIN,    NINFI,   SINFI,  NMATO,
     2                   SOUT,   NINFO,  SINFO,   IDSMI,  IDSII,
     3                   IDSMO,  IDSIO,  NTOT,    NSUBS,  IDXSUB,
     4                   ITYPE,  NINT,   INT,     NREAL,  REAL,
     5                   IDS,    NPO,    NBOPST,  NIWORK, IWORK,
     6                   NWORK,  WORK,   NSIZE,   SIZE,   INTSIZ,
     7                   LD   )
C
      use CommonDef
C
      IMPLICIT NONE
C
C     DECLARE VARIABLES USED IN DIMENSIONING
C
      INTEGER NMATI, NINFI, NMATO, NINFO, NTOT,
     +        NSUBS, NINT,  NPO,   NIWORK,NWORK,
     +        NSIZE
C
#include "ppexec_user.cmn"
      EQUIVALENCE (RMISS, USER_RUMISS)
      EQUIVALENCE (IMISS, USER_IUMISS)
C
      real*8 B(1)
#include "dms_plex.cmn"
      EQUIVALENCE(B(1), IB(1))
C
#include "dms_ncomp.cmn"
C
C     DECLARE ARGUMENTS
C
      INTEGER IDSMI(2,NMATI),      IDSII(2,NINFI),
     +        IDSMO(2,NMATO),      IDSIO(2,NINFO),
     +        IDXSUB(NSUBS),ITYPE(NSUBS), INT(NINT),
     +        IDS(2,3),     NBOPST(6,NPO),
     +        IWORK(NIWORK),INTSIZ(NSIZE),NREAL, LD,    I
      INTEGER KH2O
      REAL*8 SIN(NTOT,NMATI),     SINFI(NINFI),
     +       SOUT(NTOT,NMATO),    SINFO(NINFO),
     +       WORK(NWORK),  SIZE(NSIZE)
C     Declare Aspen I/O functions
      INTEGER USRUTL_GET_INT_PARAM, USRUTL_GET_REAL_PARAM,
     +        USRUTL_SET_INT_PARAM, USRUTL_SET_REAL_PARAM 
C
C     DECLARE LOCAL VARIABLES
C
      INTEGER IMISS, DMS_KFORMC, INDEX, IERR, IFAIL
      REAL*8 REAL(NREAL),  RMISS, WATER
C     Index for locating the particle size distribution (psd)
      INTEGER LPSD, NSI, LSLIM
C
C     Declare the variables used in model calculation
      INTEGER OPT(2)
      REAL*8 OSIZE, PITCH, COEFF(8), SA, ETA
C     BEGIN EXECUTABLE CODE
C     Get the GLCA parameters
      IERR = USRUTL_GET_INT_PARAM('OPT0', INDEX, OPT(1))
      IF (IERR .NE. 0) THEN
        WRITE(USER_NHSTRY, *) 'ERROR FETCHING RUNNING OPTION 1'
        IFAIL = 1
      END IF
      IERR = USRUTL_GET_INT_PARAM('OPT1', INDEX, OPT(2))
      IF (IERR .NE. 0) THEN
        WRITE(USER_NHSTRY, *) 'ERROR FETCHING RUNNING OPTION 2'
        IFAIL = 1
      END IF
      IERR = USRUTL_GET_REAL_PARAM('OSIZE', INDEX, OSIZE)
      IF (IERR .NE. 0) THEN
        WRITE(USER_NHSTRY, *) 'ERROR FETCHING ORIFICE SIZE'
        IFAIL = 1
      END IF
      IERR = USRUTL_GET_REAL_PARAM('PITCH', INDEX, PITCH)
      IF (IERR .NE. 0) THEN
        WRITE(USER_NHSTRY, *) 'ERROR FETCHING PITCH'
        IFAIL = 1
      END IF
      IERR = USRUTL_GET_REAL_PARAM('COEFF1', INDEX, COEFF(1))
      IF (IERR .NE. 0) THEN
        WRITE(USER_NHSTRY, *) 'ERROR FETCHING MODEL COEFFICIENT 1'
        IFAIL = 1
      END IF            
      IERR = USRUTL_GET_REAL_PARAM('COEFF2', INDEX, COEFF(2))
      IF (IERR .NE. 0) THEN
        WRITE(USER_NHSTRY, *) 'ERROR FETCHING MODEL COEFFICIENT 2'
        IFAIL = 1
      END IF 
      IERR = USRUTL_GET_REAL_PARAM('COEFF3', INDEX, COEFF(3))
      IF (IERR .NE. 0) THEN
        WRITE(USER_NHSTRY, *) 'ERROR FETCHING MODEL COEFFICIENT 3'
        IFAIL = 1
      END IF 
      IERR = USRUTL_GET_REAL_PARAM('COEFF4', INDEX, COEFF(4))
      IF (IERR .NE. 0) THEN
        WRITE(USER_NHSTRY, *) 'ERROR FETCHING MODEL COEFFICIENT 1'
        IFAIL = 1
      END IF 
      IERR = USRUTL_GET_REAL_PARAM('COEFF5', INDEX, COEFF(5))
      IF (IERR .NE. 0) THEN
        WRITE(USER_NHSTRY, *) 'ERROR FETCHING MODEL COEFFICIENT 1'
        IFAIL = 1
      END IF 
      IERR = USRUTL_GET_REAL_PARAM('COEFF6', INDEX, COEFF(6))
      IF (IERR .NE. 0) THEN
        WRITE(USER_NHSTRY, *) 'ERROR FETCHING MODEL COEFFICIENT 1'
        IFAIL = 1
      END IF 
      IERR = USRUTL_GET_REAL_PARAM('COEFF7', INDEX, COEFF(7))
      IF (IERR .NE. 0) THEN
        WRITE(USER_NHSTRY, *) 'ERROR FETCHING MODEL COEFFICIENT 1'
        IFAIL = 1
      END IF 
      IERR = USRUTL_GET_REAL_PARAM('COEFF8', INDEX, COEFF(8))
      IF (IERR .NE. 0) THEN
        WRITE(USER_NHSTRY, *) 'ERROR FETCHING MODEL COEFFICIENT 1'
        IFAIL = 1
      END IF 
C
C     Get the psd of SIN(*,2)
      CALL SHS_LOCPSD(LD, 2, LPSD, NSI, LSLIM)
C     Write the influent psd in report for check
      DO I = 1, NSI+1
        WRITE(USER_NRPT, *) 'I, SLIM(I) ', I, B(LSLIM+I-1)
      END DO
      DO I = 1, NSI+1
        WRITE(USER_NRPT, *) 'I, FRAC(I) ', I, SIN(LPSD+I-1,2)
      END DO
C     Copy the influent to effluent
      DO 100 I = 1, NTOT
        SOUT(I,1) = SIN(I,1)
        SOUT(I,2) = SIN(I,2)
 100  CONTINUE
C     Reform the psd of SOUT(*,2)
      DO I = 1, NSI+1
        SOUT(LPSD+I-1,2) = 0.1
      END DO
C
      SA = TWO*PI/(dsqrt(three)*(PITCH/OSIZE)**TWO*OSIZE)
      ETA = 1.0
C     Set the calculated results in GLCA
      IERR = USRUTL_SET_REAL_PARAM('SA', INDEX, SA)
      IF (IERR .NE. 0) THEN
        WRITE(USER_NHSTRY, *) 'ERROR STORING MONITOR VARIABLE 2'
        IFAIL = 1
      END IF
      IERR = USRUTL_SET_REAL_PARAM('ETA', INDEX, ETA)
      IF (IERR .NE. 0) THEN
        WRITE(USER_NHSTRY, *) 'ERROR STORING MONITOR VARIABLE 2'
        IFAIL = 1
      END IF
 999  RETURN
      END


