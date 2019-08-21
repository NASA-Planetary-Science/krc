C_Titl  STACOM.INC   common for STATE flags and Sun position
      LOGICAL KM,BARY
      DOUBLE PRECISION PVSUN(6)

      COMMON /STCOMX/ KM,BARY,PVSUN
C          KM   Logical flag defining physical units of the output states.
C                      KM = .TRUE., km and km/sec
C                         = .FALSE., AU and AU/day
C               Default value = .FALSE.  (KM determines time unit
c               for nutations and librations.  Angle unit is always radians.)
C
C        BARY   Logical flag defining output center.
C               Only the 9 planets are affected.
C                        BARY = .TRUE. =\ center is solar-system barycenter
C                             = .FALSE. =\ center is sun
C               Default value = .FALSE.
C
C       PVSUN    The barycentric position and velocity of the Sun.
C_Hist  2000dec08  Hugh_H_Kieffer extract from   state.f 
C  2006jan21 HK Change dimension of PVSUN from (3,2)
C_End __________________________________________________________________________
