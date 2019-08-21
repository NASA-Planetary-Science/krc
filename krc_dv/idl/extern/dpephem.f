      SUBROUTINE DPEPHEM (ET,LIST,PV,PNUT,SUN)
C_Titl  DPEPHEM  interface to JPL Planetary and Lunar Ephemerides for IDL
      IMPLICIT NONE
C_Vars
      INCLUDE 'stacom.inc'      ! /STCOMX/ KM,BARY,PVSUN
      INCLUDE 'ephcom.inc'      ! /EPHHDR/ CVAL,SS,AU,EMRAT,NUMDE,NCON,IPT
C_Args 
      DOUBLE PRECISION ET       ! In. Ephemeris time. =0 does setup only.
      INTEGER*4 LIST(12)        ! In. Specifying what interpolation wanted
C                         LIST(I)=0, no interpolation for body I
C                                =1, position only
C                                =2, position and velocity
      DOUBLE PRECISION  PV(6,12) ! out.  Requested quantities. X,Y,Z,DX/dt,DY,DZ.
      DOUBLE PRECISION  PNUT(4) ! both. Normally Output: 
C Nutations and rates, depending on the setting of LIST(11). See state.f
C  PNUT[1] = 1.1D6 (physically impossible) indicates input ET out-of-range
C  PNUT[1] = 3.0D6 indicates file open error, transferred from STATE
C If ET is 0., PNUT input:  Set (1) <-1.1D6 for Heliocentric, else Barycentric
C    Default=Native is    ! Set (2) <-1.1D6 for A.U., else KM
C    Barycentric km       ! Set (3) <-1.1D6 to reopen a file
C    Output PNUT(2) as DE_number, (3) as AU and PNUT(4) as EMRAT
      DOUBLE PRECISION  SUN(3)  ! out.  X,Y,Z of Sun re.  barycenter
C If ET is 0. SUN will return first,last,delta Julian date of source file
C_Desc
C JPL code does a Fortran STOP within STATE.f if requested time is outside
C the range of currently-loaded tables. This causes IDL to exit.
C To avoid that, test done here, with return of impossible  PNUT value.
C_Hist 2003feb09 Hugh Kieffer
C 2004jun04 HK Use PNUT input to set BARY and KM flags
C 2009mar22 HK Reset KM and BARY only when ET=0. Minor cleanup.
C D.... .WR <-> ......WR  D 6 blanks <--> 6 blanks
C_End6789012345678901234567890123456789012345678901234567890123456789012_4567890

      DOUBLE PRECISION ET2(2)   ! Ephemeris time, word 2 is dummy
      DOUBLE PRECISION ZERO
      INTEGER*4  I              ! local variables
      LOGICAL FIRST /.TRUE./ ! insure STATE is called upon first call here
      SAVE FIRST

      zero=0.0D0
      ET2(2)=zero

      IF (FIRST .OR. (ET.EQ.ZERO)) THEN ! ensure STATE has defined values for SS
         ET2(1)=ZERO            ! State will not do position calculations
                                ! setup actions
         BARY=.TRUE.            ! Force barycentric output by  STATE (native)
         KM=.TRUE.              ! coordinates in km (native to files)
         IF (PNUT(1) .LT. -1.1D6) BARY=.FALSE.
         IF (PNUT(2) .LT. -1.1D6) KM=.FALSE.
         IF (FIRST.OR. PNUT(3) .LT. -1.1D6) CALL STATE(ET2,LIST,PV,PNUT) 
         DO I=1,3               ! transfer ET limits to output
            SUN(I)=SS(I)
         ENDDO
         PNUT(2)=NUMDE          ! DE version
         PNUT(3)=AU             ! return some constants
         PNUT(4)=EMRAT
         FIRST=.FALSE. 
      ENDIF

      IF (ET.NE.ZERO) THEN      ! try for positions
         IF (ET.LT.SS(1) .OR. ET.GT.SS(2)) THEN ! out of range
            WRITE(*,198)ET,SS(1),SS(2)
 198        format(' ***  Requested ET,',f12.2,
     *       ' not within ephemeris limits,',2f12.2,'  ***')
            PNUT(1)= 1.1D6      ! impossible value as a flag
         ELSE                   ! this is the normal action
            ET2(1)=ET           ! construct 2 element time for  STATE
            CALL STATE(ET2,LIST,PV,PNUT) ! get the positions
            DO I=1,3            ! transfer Sun position to output
               SUN(I)=PVSUN(I)
            ENDDO
            
         ENDIF
      ENDIF

      RETURN
      END
