      REAL*8 FUNCTION GASPT (KODE, ARG2)
C_Titl  GASPT  Gas saturation pressure / temperature relation
C_Vars
      INCLUDE 'krcc8m.f'  ! has  IMPLICIT NONE 
C_Args
      INTEGER KODE            !in. 1=P-to-T  2=T-to-P
      REAL*8 ARG2               !in. pressure (pascal) or temperature (kelvin)
CC REAL  GASPT !func. temperature or pressure
C_Desc
C  Clausius-Clapeyron equation:
C  ln  P = a -  b/T   or  T = b/(a - ln  P)
C_Lien  Minimum temperature of 10 K to avoid dealing with potential 
C      numerical failure
C_Hist  Hugh_Kieffer 2015sep14  Adopt from co2pt.f
C_End6789012345678901234567890123456789012345678901234567890123456789012_4567890

C      REAL A   /27.9546/  ! first  C-C coefficient for  CO2 (mb) mars p 959
C      REAL B   /3182.48/  ! second  C-C coefficient for  CO2  (1/K)
      REAL*8 OUT,P, T
      REAL*8 A,B

C transfer two items from KRCCOM explicitly for clarity. 
C as of 2015sep14 these are the only two spare Real input parameters
      A=ABRPHA                  ! first Clausius-Clapeyron coefficient
      B=FD32                    ! second  " " "  
C      print *, 'A,B=',A,B,KODE,arg2
      IF (KODE.EQ.1) THEN
        P=ARG2
        IF (P.LE.0.) P=1.  ! dumb insurance to avoid log failure
        OUT = B/(A-LOG(P))
      ELSE
        T=ARG2
        IF (T.LT.10.) T=10. ! dumb insurance to avoid numerical failure
        OUT = EXP( A-B/T)
      ENDIF
      GASPT = OUT
      RETURN
      END
