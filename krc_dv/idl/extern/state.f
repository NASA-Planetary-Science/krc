      SUBROUTINE STATE(ET2,LIST,PV,PNUT)
C_Titl  STATE   Reads and interpolates the JPL planetary ephemeris file
      IMPLICIT NONE
      SAVE
C_Args
      DOUBLE PRECISION ET2(2)   ! in.  JULIAN EPHEMERIS EPOCH AT WHICH 
C                INTERPOLATION IS WANTED.  ANY COMBINATION OF ET2(1)+ET2(2) 
C                WHICH FALLS WITHIN THE TIME SPAN ON THE FILE IS A 
C                PERMISSIBLE EPOCH. See discussion below
C                If ET2(1)=0., then will reopen file and return. Other values fatal
      INTEGER LIST(12)          ! in. ARRAY SPECIFYING WHAT INTERPOLATION
C               IS WANTED FOR EACH OF THE BODIES ON THE FILE. See Table below.
C                         LIST(I)=0, NO INTERPOLATION FOR BODY I
C                                =1, POSITION ONLY
C                                =2, POSITION AND VELOCITY
      DOUBLE PRECISION PV(6,12) ! out. The REQUESTED INTERPOLATED
C               QUANTITIES.  THE BODY SPECIFIED BY LIST(I) WILL HAVE ITS
C               STATE IN THE ARRAY STARTING AT PV(1,I).  (ON ANY GIVEN
C               CALL, ONLY THOSE WORDS IN 'PV' WHICH ARE AFFECTED BY THE
C               FIRST 10 'LIST' ENTRIES (AND BY LIST(12) IF LIBRATIONS ARE
C               ON THE FILE) ARE SET.  THE REST OF THE 'PV' ARRAY
C               IS UNTOUCHED.)  THE ORDER OF COMPONENTS STARTING IN
C               PV(1,I) IS: X,Y,Z,DX,DY,DZ.
C
C               ALL OUTPUT VECTORS ARE REFERENCED TO THE EARTH MEAN
C               EQUATOR AND EQUINOX OF J2000 IF THE DE NUMBER IS 200 OR
C               GREATER; OF B1950 IF THE DE NUMBER IS LESS THAN 200. 
C
C               THE MOON STATE IS ALWAYS GEOCENTRIC; THE OTHER NINE STATES 
C               ARE EITHER HELIOCENTRIC OR SOLAR-SYSTEM BARYCENTRIC, 
C               DEPENDING ON THE SETTING OF COMMON FLAGS (SEE BELOW).
C
C               LUNAR LIBRATIONS, IF ON FILE, ARE PUT INTO PV(K,11) IF
C               LIST(12) IS 1 OR 2.
      DOUBLE PRECISION PNUT(4) ! out.  WILL CONTAIN NUTATIONS AND RATES,
C               DEPENDING ON THE SETTING OF LIST(11).  THE ORDER OF
C               QUANTITIES IN NUT IS:
C
C                        D PSI     (NUTATION IN LONGITUDE)
C                        D EPSILON (NUTATION IN OBLIQUITY)
C                        D PSI DOT
C                        D EPSILON DOT
C              If file failure, returns PNUT(1)=-3.d0
C
C_Vars
      INCLUDE 'chrcom.inc'      ! /CHRHDR/ CNAM,TTL
      INCLUDE 'stacom.inc'      ! /STCOMX/ KM,BARY,PVSUN
      INCLUDE 'ephcom.inc'      ! /EPHHDR/ CVAL,SS,AU,EMRAT,NUMDE,NCON,IPT
C_Desc
C Assumes that test for valid ET done before call to this routine.
C only error test left is reading off end of file
C         ET2 options
C
C                A. FOR EASE IN PROGRAMMING, THE USER MAY PUT THE
C                   ENTIRE EPOCH IN ET2(1) AND SET ET2(2)=0.
C
C                B. FOR MAXIMUM INTERPOLATION ACCURACY, SET ET2(1) =
C                   THE MOST RECENT MIDNIGHT AT OR BEFORE INTERPOLATION
C                   EPOCH AND SET ET2(2) = FRACTIONAL PART OF A DAY
C                   ELAPSED BETWEEN ET2(1) AND EPOCH.
C
C                C. AS AN ALTERNATIVE, IT MAY PROVE CONVENIENT TO SET
C                   ET2(1) = SOME FIXED EPOCH, SUCH AS START OF INTEGRATION,
C                   AND ET2(2) = ELAPSED INTERVAL BETWEEN THEN AND EPOCH.
C
C        LIST:    THE DESIGNATION OF THE ASTRONOMICAL BODIES BY I IS:
C                         I = 1: MERCURY
C                           = 2: VENUS
C                           = 3: EARTH-MOON BARYCENTER
C                           = 4: MARS
C                           = 5: JUPITER
C                           = 6: SATURN
C                           = 7: URANUS
C                           = 8: NEPTUNE
C                           = 9: PLUTO
C                           =10: GEOCENTRIC MOON
C                           =11: NUTATIONS IN LONGITUDE AND OBLIQUITY
C                           =12: LUNAR LIBRATIONS (IF ON FILE)
C
C           *   STATEMENT # FOR ERROR RETURN, IN CASE OF EPOCH OUT OF
C               RANGE OR I/O ERRORS.
C_Calls  FSIZER2  INTERP  SPLIT  May use SWAPBYTE4  SWAPBYTE8 
C
C_Hist  2006jan21  Code pulled from ssd.jpl.nasa.gov//pub/eph/export/DE405/
C   Original code by Myles Standish of JPL.    Extract routine from testeph.f
C 2006jan21 Hugh_H_Kieffer  Use IMPLICIT NONE, rearrange argument documentation
C 2009mar23 HK Revise action and tests for ET2(1) EQ 0.
C D......WR <-> ......WR  D 6 blanks <--> 6 blanks
C 2009jun28 HK  deactivate swap-bytes capability Add error prompt for file failure
C_End6789012345678901234567890123456789012345678901234567890123456789012_4567890
C
      DOUBLE PRECISION T(2) ! ephemeris time is the sum of these
      DOUBLE PRECISION PJD(4)
      DOUBLE PRECISION BUF(1500) ! to hold Cheb. Coeff for an ephemeris interval
      DOUBLE PRECISION AUFAC, S,TMP1,TMP2
      INTEGER I,IRECSZ,J,K,KSIZE,NCOEFFS,NR,NRECL,NRFILE,NRL
      LOGICAL FIRST
      DATA FIRST/.TRUE./

      CHARACTER*80 NAMFIL

      PNUT(1)=0.D0 ! OK return, in case  PNUT not calculated
C
C       ENTRY POINT - 1ST TIME IN, GET POINTER DATA, ETC., FROM EPH FILE
C
      IF (FIRST .OR. ET2(1).EQ.0.D0) THEN
         IF (.NOT. FIRST) CLOSE (NRFILE)
        FIRST=.FALSE.

C ************************************************************************
C ************************************************************************

C THE USER MUST SELECT ONE OF THE FOLLOWING BY DELETING THE 'C' IN COLUMN 1

C ************************************************************************

C        CALL FSIZER1(NRECL,KSIZE,NRFILE,NAMFIL)
D      write (*,*) 'HHK In STATE just before FSIZER call'
        CALL FSIZER2(NRECL,KSIZE,NRFILE,NAMFIL)
C        CALL FSIZER3(NRECL,KSIZE,NRFILE,NAMFIL)
D      write (*,*) 'HHK In STATE just after  FSIZER call'
        WRITE(*,*)'NRECL,KSIZE,NRFILE,NAMFIL',NRECL,KSIZE,NRFILE,NAMFIL

      IF(NRECL .EQ. 0 .OR. KSIZE.EQ.0) THEN
         WRITE(*,*)'  ***** FSIZER IS NOT WORKING *****'
         PNUT(1)=3.D6           ! impossible value as flag
         RETURN
      ENDIF
C ************************************************************************
C ************************************************************************

      IRECSZ=IABS(NRECL)*KSIZE ! use IABS because -NRECL is flag for swap
      NCOEFFS=KSIZE/2
       WRITE(*,*) 'NRECL,IRECSZ,NCOEFFS=',NRECL,IRECSZ,NCOEFFS

        OPEN(NRFILE,
     *       FILE=NAMFIL,
     *       ACCESS='DIRECT',
     *       FORM='UNFORMATTED',
     *       RECL=IRECSZ,ERR=88,  ! HK add error label
     *       STATUS='OLD')
C  Word types:           ch   ch f8  int f8    f8  hhk
      READ(NRFILE,REC=1)TTL,CNAM,SS,NCON,AU,EMRAT,
     . ((IPT(I,J),I=1,3),J=1,12),NUMDE,(IPT(I,13),I=1,3) ! all Integer   hhk
D         write(*,*)'STATE: after read 1: emrat=',emrat ! hhk
      READ(NRFILE,REC=2)CVAL
C Next section added by HHK to deal with swapped bytes.
D         write(*,*)'STATE: after read 2' 
C-      IF(NRECL.LT.0) THEN ! swap bytes in place 
C-      WRITE(*,*)'Swapping'
C-         CALL SWAPBYTE8(SS,3,SS)
C-         CALL SWAPBYTE8(AU,1,AU)
C-         CALL SWAPBYTE8(EMRAT,1,EMRAT)
C-         CALL SWAPBYTE8(CVAL,400,CVAL)
C-         CALL SWAPBYTE4(NCON,1,NCON)
C-         CALL SWAPBYTE4(NUMDE,1,NUMDE)
C-         CALL SWAPBYTE4(IPT,39,IPT)
C-      ENDIF
      WRITE(*,*)'NCON,AU,EMRAT,NUMDE=',NCON,AU,EMRAT,NUMDE

      NRL=0
      IF(ET2(1) .EQ. 0.D0) RETURN
      ENDIF


C       ********** MAIN ENTRY POINT **********

      S=ET2(1)-.5D0             ! following comment by HHK
      CALL SPLIT(S,PJD(1))      ! split offset  ET1 into integer and fraction
      CALL SPLIT(ET2(2),PJD(3)) ! split  ET2 into integer and fraction
      PJD(1)=PJD(1)+PJD(3)+.5D0 ! sum the integers plus the offset
      PJD(2)=PJD(2)+PJD(4)      ! sum the fractions
      CALL SPLIT(PJD(2),PJD(3)) ! if fraction sum were >1, this will split it
      PJD(1)=PJD(1)+PJD(3)  ! sum the integers.  PJD(4) now contains the fraction

C       CALCULATE RECORD # AND RELATIVE TIME IN INTERVAL

      NR=IDINT((PJD(1)-SS(1))/SS(3))+3
      IF(PJD(1).EQ.SS(2)) NR=NR-1

        tmp1 = DBLE(NR-3)*SS(3) + SS(1)
        tmp2 = PJD(1) - tmp1
        T(1) = (tmp2 + PJD(4))/SS(3)

C       READ CORRECT RECORD IF NOT IN CORE

      IF(NR.NE.NRL) THEN
        NRL=NR
        READ(NRFILE,REC=NR,ERR=99)(BUF(K),K=1,NCOEFFS)
      ENDIF

      IF(KM) THEN
         T(2)=SS(3)*86400.D0
         AUFAC=1.D0
      ELSE
         T(2)=SS(3)
         AUFAC=1.D0/AU
      ENDIF

C   INTERPOLATE SSBARY SUN

      CALL INTERP(BUF(IPT(1,11)),T,IPT(2,11),3,IPT(3,11),2,PVSUN)

      DO I=1,6
      PVSUN(I)=PVSUN(I)*AUFAC
      ENDDO

C   CHECK AND INTERPOLATE WHICHEVER BODIES ARE REQUESTED

      DO 4 I=1,10
      IF(LIST(I).EQ.0) GO TO 4

      CALL INTERP(BUF(IPT(1,I)),T,IPT(2,I),3,IPT(3,I),
     & LIST(I),PV(1,I))

      DO J=1,6
       IF(I.LE.9 .AND. .NOT.BARY) THEN
       PV(J,I)=PV(J,I)*AUFAC-PVSUN(J)
       ELSE
       PV(J,I)=PV(J,I)*AUFAC
       ENDIF
      ENDDO

   4  CONTINUE

C       DO NUTATIONS IF REQUESTED (AND IF ON FILE)

      IF(LIST(11).GT.0 .AND. IPT(2,12).GT.0)
     * CALL INTERP(BUF(IPT(1,12)),T,IPT(2,12),2,IPT(3,12),
     * LIST(11),PNUT)

C       GET LIBRATIONS IF REQUESTED (AND IF ON FILE)

      IF(LIST(12).GT.0 .AND. IPT(2,13).GT.0)
     * CALL INTERP(BUF(IPT(1,13)),T,IPT(2,13),3,IPT(3,13),
     * LIST(12),PV(1,11))

      RETURN

 88   Write(*,*)' STATE Failed to open file=',NAMFIL
      pnut(1)=8.d6              ! set as error flag
      return


   99 WRITE(*,'(2F12.2,A80)')ET2,' STATE  hit EOF. Will STOP'
      pnut(1)=4.d6              ! set as error flag
      return

      END
