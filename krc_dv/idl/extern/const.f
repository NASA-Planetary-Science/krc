      SUBROUTINE CONST(NAM,VAL,SSS,N)
C_Titl CONST  Obtain constants from the ephemeris file
      IMPLICIT NONE
      SAVE
C_Args
      CHARACTER*6 NAM(*)        ! out. Current array of constant names
      DOUBLE PRECISION VAL(*)   ! out. Current array of corresponding constants
      DOUBLE PRECISION SSS(3)   ! out. Current start,stop,step times of files
      INTEGER N                 ! out. number of entries in  NAM and  VAL arrays
C_Vars
      INCLUDE 'chrcom.inc'      ! /CHRHDR/ CNAM,TTL
      INCLUDE 'ephcom.inc'      ! /EPHHDR/ CVAL,SS,AU,EMRAT,NUMDE,NCON,IPT
C_Hist  2006jan21  Code pulled from ssd.jpl.nasa.gov//pub/eph/export/DE405/
C   Original code by Myles Standish of JPL.    Extract routine from testeph.f
C 2006jan21 Hugh_H_Kieffer  Use IMPLICIT NONE, rearrange argument documentation
C_End __________________________________________________________________________

      DOUBLE PRECISION zips(2),xx(99)
      data zips/2*0.d0/

      INTEGER I,list(11)
      logical first
      data first/.true./

C  CALL STATE TO INITIALIZE THE EPHEMERIS AND READ IN THE CONSTANTS

      IF (FIRST) CALL STATE(zips,list,xx,xx)
      first=.false.

      N=NCON

      DO I=1,3
        SSS(I)=SS(I)
      ENDDO

      DO I=1,N
        NAM(I)=CNAM(I)
        VAL(I)=CVAL(I)
      ENDDO

      RETURN

      END
