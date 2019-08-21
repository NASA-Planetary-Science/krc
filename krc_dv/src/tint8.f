        SUBROUTINE TINT8 (VIN, AREA, SUM)
C_Titl  TINT   spherical integrals over globe
C_Vars
        INCLUDE 'krcc8m.f'      ! contains  IMPLICIT NONE statement. 
C   Uses  RADC  N4  ALAT = band latitudes <degrees>
C_Args
        REAL*8 VIN(*)   ! in. values representative of each latitude band
        REAL*8 AREA(*)  ! in/out. normalized area of each band
        REAL*8 SUM      ! out. global average 
C_Lims
C  Latitudes in  ALAT must increase (from south to north), and should cover 
C that part of the planet where  VIN will be non-zero.
C_Desc 
C  Starts at south pole, and places boundaries between the bands at the
C point halfway between successive entries in  ALAT.  The weighting functions
C are computed on an intializing call with  AREA(1) = 0. on input.
C_Hist  84jun12  Hugh_H_Kieffer  U.S.G.S._Flagstaff original version
C   87sep23 HK revise documentation
C   93mar03 ECisneros Ported to UNIX; made include filenames lowercase
C   97jan30 HK minor edits
C 2010jan12 HK Use IMPLICIT NONE
C 2011aug01 HK Fix incorrect typing
C 2014mar10 HK Make  REAL*8  version.
C 2014may04 HK Untabify
C_End
        INTEGER*4 I
        REAL*8 EDGE,SL,S
Cd      print * , 'TINT: N4',N4,MAXN4 !<dbug
        IF (AREA(1).LE.0.) THEN ! first time, compute area array
          SL = -1.0D0           ! sine of prior band edge (south pole at start)
          DO  I = 1,N4
            IF (I.LT.N4) THEN
              EDGE = (ALAT(I)+ALAT(I+1))/2.D0 ! halfway between
            ELSE
              EDGE = 90.D0      ! north pole for last edge
            ENDIF
            S = DSIN (EDGE/RADC)
            AREA(I) = (S-SL)/2.D0       ! normalized area for this band
            SL = S
          ENDDO
C         write(*,*) 'Radc=',radc
C         write(*,*) 'alat=',(alat(i),i=1,n4)
        ENDIF
C       write(*,*) 'area=',(area(i),i=1,n4)
        SUM = 0.D0              ! do summing weighted by area
        DO I = 1, N4
          SUM = SUM + AREA(I) * VIN(I)
        ENDDO
        RETURN
        END
