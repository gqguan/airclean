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
      REAL*8 REAL(NREAL), RMISS, WATER
C     Index for locating the particle size distribution (psd)
      INTEGER LPSD, NSI, LSLIM
C     Temporary variables
      INTEGER KDIAG, NCP, IDX(10)
      REAL*8 FLOW, X(10), Y(10)
C
C     Declare the variables used in model calculation
      INTEGER OPT(2)
      REAL*8 OSIZE, PITCH, COEFF(8), SA, ETA
      REAL*8 Temperature_gas, Temperature_liq
      REAL*8 Pressure_gas, Pressure_liq, Density_gas, Density_liq
      REAL*8 Velocity_gas, Velocity_liq, Viscosity_gas, Viscosity_liq
      REAL*8 D50, ContactArea, RelHumidity, Re_gas, Re_liq
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
        WRITE(USER_NHSTRY, *) 'ERROR FETCHING MODEL COEFFICIENT 4'
        IFAIL = 1
      END IF 
      IERR = USRUTL_GET_REAL_PARAM('COEFF5', INDEX, COEFF(5))
      IF (IERR .NE. 0) THEN
        WRITE(USER_NHSTRY, *) 'ERROR FETCHING MODEL COEFFICIENT 5'
        IFAIL = 1
      END IF 
      IERR = USRUTL_GET_REAL_PARAM('COEFF6', INDEX, COEFF(6))
      IF (IERR .NE. 0) THEN
        WRITE(USER_NHSTRY, *) 'ERROR FETCHING MODEL COEFFICIENT 6'
        IFAIL = 1
      END IF 
      IERR = USRUTL_GET_REAL_PARAM('COEFF7', INDEX, COEFF(7))
      IF (IERR .NE. 0) THEN
        WRITE(USER_NHSTRY, *) 'ERROR FETCHING MODEL COEFFICIENT 7'
        IFAIL = 1
      END IF 
      IERR = USRUTL_GET_REAL_PARAM('COEFF8', INDEX, COEFF(8))
      IF (IERR .NE. 0) THEN
        WRITE(USER_NHSTRY, *) 'ERROR FETCHING MODEL COEFFICIENT 8'
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
C     Calculate the median particle size
      D50 = DMEAN(B(LSLIM:(LSLIM+NSI)), SIN(LPSD:(LPSD+NSI),2))
C     Reform the psd of SOUT(*,2)
      DO I = 1, NSI+1
        SOUT(LPSD+I-1,2) = D50
      END DO
C
      KDIAG = 4
C     Calculate the liquid velocity
      Velocity_liq = 1.0
C     Get the liquid temperature
      Temperature_liq = SIN(NCOMP_NCC+2,1)
C     Get the liquid pressure
      Pressure_liq = SIN(NCOMP_NCC+3,1)
C     Get the liquid density
      Density_liq = SIN(NCOMP_NCC+8,1)            
C     Get the liquid viscosity
      CALL SHS_CPACK(SIN(1,1), NCP, IDX, X, FLOW)
      CALL PPMON_VISCL (Temperature_liq, Pressure_liq, X, NCP, IDX, 
     &                   NBOPST, KDIAG, Viscosity_liq, IERR)
C     Calculate the gas Reynolds number
      Re_liq = D50*Velocity_liq*Density_liq/Viscosity_liq
C      
C     Calculate the gas velocity
      Velocity_gas = 1.0
C     Get the gas temperature
      Temperature_gas = SIN(NCOMP_NCC+2,2)
C     Get the gas pressure
      Pressure_gas = SIN(NCOMP_NCC+3,2)
C     Get the gas density
      Density_gas = SIN(NCOMP_NCC+8,2)            
C     Get the gas viscosity
      CALL SHS_CPACK(SIN(1,2), NCP, IDX, Y, FLOW)
      CALL PPMON_VISCV (Temperature_gas, Pressure_gas, Y, NCP, IDX, 
     &                   NBOPST, KDIAG, Viscosity_gas, IERR)
C     Calculate the gas Reynolds number
      Re_gas = D50*Velocity_gas*Density_gas/Viscosity_gas
C
C     Write Re in report for check
      WRITE(USER_NRPT, '(A, F8.2, 4E10.3)') 'Liq T P rho VISC Re ', 
     & Temperature_liq, Pressure_liq, Density_liq, Viscosity_liq, Re_liq
      WRITE(USER_NRPT, '(A, F8.2, 4E10.3)') 'Gas T P rho VISC Re ', 
     & Temperature_gas, Pressure_gas, Density_gas, Viscosity_gas, Re_gas
      WRITE(USER_NRPT, '(A, E10.3)') 'D50 = ', D50
C
C     Calculate contact area
      ContactArea = 1.0
C     Calculate relative humidity
      RelHumidity = 0.5
C
C     Calculate the dedusty efficient
      ETA = 1.0 - COEFF(1)*Re_gas**COEFF(2)*Re_liq**COEFF(3)*
     &      (ContactArea*D50)**COEFF(4)*RelHumidity**COEFF(7)*
     &      (Velocity_gas/Velocity_liq)**(COEFF(3)-COEFF(5))*
     &      (Density_gas/Density_liq)**(COEFF(3)-COEFF(6))
C
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


