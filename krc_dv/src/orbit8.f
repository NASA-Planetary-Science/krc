      SUBROUTINE ORBIT8 (SMA,PERIOD,ECC,TPER, RADF,XX)
C_TITL  ORBIT  Compute radius and coordinates for elliptical orbit. DefPrec.
      IMPLICIT NONE
C_ARG
      REAL*8 SMA        !I. semimajor axis, in units desired for  XX.
      REAL*8 PERIOD       !I. orbital period in same units as  TPER
      REAL*8 ECC       !I. eccentricity of orbit
      REAL*8 TPER      !I. request time from periapsis, same units as  PERIOD
      REAL*8 RADF      !O. current distance from focus, in units of  SMA.
C                     Will be set negative if eccentric anomoly 
C                     computation not fully accurate. 
      REAL*8 XX(3)      !O. cartesian position of body in system with "z" toward
C                    positive pole of orbit, "x" toward periapsis
C_Calls  ECCANOM  D[SIN,COS,SQRT,MOD]
C_Hist 84may29  Hugh_H_Kieffer  From earlier versions ~69,~78
C 1989dec11  HK  Move determination of eccentric anomoly into  ECCANOM
C 2005dec28  HK  Change to use of IMPLICIT NONE
C 2012mar02  HK  Clean up names and case. no change to algorithm
C 2013mar11 HK Make  REAL*8  version   
C_END
      REAL*8 TWOPI/6.283185307179586D0/
      REAL*8 ANOM,EE,CE
      REAL*8 ECCANOM8            ! function

      RADF=SMA            ! Temporary so it could be negated as flag
      ANOM=DMOD(TWOPI*TPER/PERIOD,TWOPI)      ! calc. mean anomoly
      EE=ECCANOM8(ECC,ANOM)                  ! get eccentric anomoly
      IF (EE-ANOM.GT.4.D0) RADF=-SMA  !  eccanom did not reach desired accuracy
      CE=DCOS(EE)
      XX(1)= SMA * (CE-ECC)
      XX(2)= SMA * DSQRT(1.D0-ECC**2) *DSIN(EE)
      XX(3)= 0.
      RADF=RADF*(1.D0-ECC*CE)      ! Heliocentric radius
      RETURN
      END
