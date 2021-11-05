      SUBROUTINE TSEAS8 (IQ,IRET)
C_Titl TSEAS Advance one "season" along planets orbit for KRC system
C_Vars
      INCLUDE 'krcc8m.f'      ! has IMPLICIT NONE
      INCLUDE 'latc8m.f'
      INCLUDE 'hatc8m.f'
      INCLUDE 'porbc8m.f'   ! need for only OPERIOD
      INCLUDE 'unic8m.f'
      INCLUDE 'filc8m.f'    ! need FFAR
C_Args
      INTEGER*4 IQ     ! in. 1 = start from scratch
C                            2 = restart from disk
C                            3 = continue from current conditions
      INTEGER*4 IRET   ! return code, may be from lower routine
C_Desc
C if IQ =3, then continuing from existing model, and adopt its last date
C_Hist 97feb11  Hugh  Kieffer
C
C_Calls  PORBIT  SEASALB  SEASTAU  TCARD8  TDAY8  TDISK8  TINT8  TLATS8  TPRINT8 
C_Limits
C Use of far-field file (fff): Assumes that fff is for the same body as current 
C run. Expects that fff covers all of the seasons for current run;  if requested 
C season is beyond those in file by less than one, will wrap to first season.
C If beyond my 1 or more, will cause a failure, return with IRL=3    
C   97aug22 HK Calculated julian day by multiplication, not addition
C 2002nov01 HK Have DJU5 increment by current DELJUL for each season
C 2008oct02 HK Replace ID22(1) and (2) with KVALB and KVTAU
C 2008oct22 HK Used IDOWN as season to make changes
C 2009mar05 HK Minor cleanup
C 2009apr22 HK Add call to TYEAR
C 2010jan12 HK Use IMPLICIT NONE, move TINT call from TLATS to TSEAS
C 2011aug07 HK Change test to call PORB from N5.GT.0
C 2012mar02 HK Add atmosphere flag, remove unused labels
C 2012mar27 HK Test on KVTAU changed from .GE.0 to EQ.1
C 2014jan24 HK Compute date with multiply rather than repeated adds
C 2014feb05 HK LKEY true will compute date of first season 
C 2014feb25 HK Specify most variables as *4  Untabify and justify
C 2014mar10 HK Make  REAL*8  version 
C 2014may16:aug22 HK Incorporate option of far-field file for slopes
C 2016oct10 HK Make print after TFAR optional
C 2017mar05 HK Improve notice when there is an error
C 2017apr29 HK Define SJA if not using PORB
C 2018nov05 HK Prepend D to lines activated by IDBx
C_End 789012345678901234567890123456789012345678901234567890123456789012_4567890
C 
      INTEGER*4 I1,I2,IRL,KREC
      INTEGER*4 MEMI(8), MEMA(8) ! integer information from fff surf/atm
      REAL*8 DJONE              ! first date
      REAL*8 BUF(MAXN4)         ! fractional surface area in each latitude zone
      REAL*8 FFTOL /0.01d0/     ! Tolerance on fff season interpolation
      REAL*8 FFOLE              ! transfer tolerance to TFAR, return error code
      REAL*8 DUM8               ! dummy scalar
      REAL*8 DU8H4(NUMH4)    ! MAXNH*MAXN4 dummy, OK for arrays up to this size
      REAL SEASALB,SEASTAU      ! functions called
      REAL*4 ALB4,SUBS4,TAUD4   ! for *4*8 conversion 
      REAL*4 TIME1,TIME2,TIME3 

      LATM=PTOTAL.GT.1.         ! atmosphere present flag
      IRET=1                    ! normal
D     IF (IDB1.NE.0) WRITE(IOSP,*) 'TSEASa',IQ,J5,LSC,N5,LONE

      IF (IQ .EQ. 1) THEN       ! if start of a model
        J5=0                    ! initialize season counter
C System_Clock is millisec or finer
C DTIME is real seconds
C CPU_TIME is real with resolution microseconds
        CALL CPU_TIME(TIME1)
        SUMF=0.                 ! global seasons frost
      ENDIF
      BUF(1)=0.                 ! flag for  TINT to compute areas

      IF (LKEY) THEN
        CALL PORBIT(2,DJU5,DJUL,SDEC,DAU) ! DJU5 will be the MJD for Ls=DJUL
        DJONE=DJU5-(JDISK-1)*DELJUL ! starting date to reach Ls at Jdisk
      ELSE
        DJONE=DJUL              ! starting date as input
      ENDIF
      YRIDAY=OPERIOD            ! transfer length of year into  KRCCOM


C VVVVVVVVVVVVVVVVVVVVVVVVVV new season loop VVVVVVVVVVVVVVVVVVVVVVVVVV
C
 100  DJU5=DJONE+FLOAT(J5)*DELJUL ! current julian date
      J5=J5+1                   ! increment season index
      IF (LSC .OR. J5.EQ.IDOWN) THEN !  LSC  invokes changes at each season
        CALL TCARD8 (2,IRL)       ! read parameter change cards  
        IRET=IRL                !  IRL range is 1:7
        IF (IRL.GE.4) THEN
          WRITE (IOERR,*) 'TSEAS: TCard(2 return=',IRL 
          IF (IRL .GT. 5) IRET=10+IRL ! cannot continue this case
          GOTO 9             ! either 1-point or end-of-data
        ENDIF
D     write (IOSP,*)'TSEAS: ',J5,N5,LOPN2,IDOWN,IRL !<< ,KVALB,alb
        CALL TDAY8 (1,IRL)        ! re-initialize day computations
        IF (IRL.NE.1) THEN
          WRITE (IOERR,*) 'TSEAS: TDAY(1 error=',IRL 
          IRET=20+IRL ! cannot continue this case
          GOTO 9
        ENDIF
      ENDIF
      IF (MOD(J5,NMOD).EQ.0) THEN ! notify terminal
        CALL CPU_TIME(TIME2)
        TIME3=TIME2-TIME1
        WRITE(IOPM,115) TIME3,'start',J5,N5
 115    FORMAT(1X,F9.3,' seconds at ',A,' of season ',I4,' of ',I4)
      ENDIF
      IF (LPORB) THEN
        CALL PORBIT (1, DJU5,SUBS,SDEC,DAU) ! Get current Ls, AU and sub-sol lat
      ELSE
        SJA=DAU                 ! TLATS uses SJA for annual average
      ENDIF
C
      SUBS4=SNGL(SUBS)                ! get R*4 version
      IF (KVALB .GT. 0) THEN
        ALB4=SEASALB(SUBS4)     ! variable soil albedo
        ALB=ALB4                ! move into R*8
      ENDIF
      IF (KVTAU .EQ. 1) THEN 
        TAUD4=SEASTAU(SUBS4)    ! variable atm. opacity
        TAUD=TAUD4
      ENDIF

Cvvvvvvvvvvvvvvvvvvvvvv start: Get date in fff vvvvvvvvvvvvvvvvvvvvvvvvvvv
      IF (LOPN3 ) THEN ! fetch far field temperatures for this season
        FFOLE=FFTOL             ! set tolerance
        CALL TFAR8(3,DJU5,FFOLE,MEMI, FARTS(1,1,1),DU8H4,FARTS(1,1,2)) !surf
        I1=MEMI(1)/2 ! near midday
        I2=MEMI(2)/2 ! near the equator
 130    FORMAT ('Far near-noon near-equator [Ts and] Ta',2I4,2g11.4)
D       IF (IDB6.GE.5) WRITE(IOSP,130) I1,I2,FARTS(I1,I2,1)
D    &    ,FARTS(I1,I2,2)

        IF (FFOLE .LT. 0.D0) THEN 
          IRET=40+NINT(-FFOLE)              ! signal an error
          GOTO 9
        ENDIF
        IF (LATM .AND. .NOT.LFAME) THEN ! still need Tatm
          DUM8=FFTOL            ! set tolerance
          CALL TFAR8(13,DJU5,DUM8,MEMA, DU8H4,DU8H4,FARTS(1,1,2)) ! Tatm
          I1=MEMA(1)/2          ! near midday
          I2=MEMA(2)/2          ! near the equator
           IF (IDB6.GE.3) WRITE(IOSP,130)I1,I2,FARTS(I1,I2,2)
          IF (DUM8 .LT. 0.D0) THEN 
            IRET=40+NINT(-DUM8)  ! signal an error
            GOTO 9
          ENDIF
        ENDIF                   ! if LATM
      ENDIF                     ! LOPN3 .AND. SLOPE .GT. 0
C^^^^^^^^^^^^^^^^^^^^^^^^ end: Get date in fff^^^^^^^^^^^^^^^^^^^^^^^^^^^

D      write (IOSP,*)'TSEAS: ',J5,N5,LOPN2,IDOWN,SUBS ! ,KVALB,alb
C======

      CALL TLATS8 (IQ,IRL)       ! execute latitude loop

C======
      IF (IRL.NE.1) THEN   ! range is 31:39
        WRITE(IOERR,*)' TSEAS:  TLAT error=',IRL 
        IRET=IRL             ! cannot continue this case
      ENDIF
C If there was a blowup, IRL will be 2  and will exit the season loop 
      IF (N4.GT.8 .AND. LATM) CALL TINT8 (FROST4, BUF, SUMF) !  MKS units  BUF = normalized area 
      IF (LP5) CALL TPRINT8 (5)  ! print latitude summary
      IF (LP6) CALL TPRINT8 (6)  !  & min and max layer temperature
      IF (J5.EQ.JDISK .AND. KOLD.EQ.0 .AND. LOPN2 ! write  KRCCOM record
     &  .AND. K4OUT.LT.0) CALL TDISK8 (5,KREC) ! must follow tday(1)
      IF (J5.GE.JDISK) CALL TDISK8 (2,KREC) ! write this season
C
C Check if more seasons and no blowup
      IF (J5.LT.N5 .AND. IRL.EQ.1) GO TO 100
C AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA  end season loop AAAAAAAAAAAAAAAAAAAAAAAAAAAAA
      IF (.NOT.LONE) THEN
        CALL CPU_TIME(TIME2)
        TIME3=TIME2-TIME1
        WRITE(IOPM,115) TIME3,'end',J5,N5
        WRITE(IOSP,115) TIME3,'end',J5,N5
      ENDIF  
      IRET=IRL

9     RETURN
      END
