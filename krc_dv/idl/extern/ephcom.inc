C_Titl  EPHCOM.INC   common for Ephemeris header itesm
      DOUBLE PRECISION CVAL(400) ! constants
      DOUBLE PRECISION SS(3)    ! File limits of Ephemeris Time as Julian Date
C                     START, STOP, STEP OF EPHEMERIS
      DOUBLE PRECISION AU       ! Astronomical Unit in km
      DOUBLE PRECISION EMRAT    ! Earth/Moon mass ratio
      INTEGER NUMDE
      INTEGER NCON
      INTEGER IPT(3,13)
C  All values are set in  state.f
      COMMON /EPHHDR/ CVAL,SS,AU,EMRAT,NUMDE,NCON,IPT
C_Hist  2000dec08  Hugh_H_Kieffer extract from   state.f 
C_End __________________________________________________________________________
