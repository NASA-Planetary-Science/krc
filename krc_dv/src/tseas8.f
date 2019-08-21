      SUBROUTINE TSEAS8 (IQ,IRET)
C_Titl TSEAS Advance one "season" along planets orbit for KRC system
C_Vars
      INCLUDE 'krcc8m.f'      ! has IMPLICIT NONE
      INCLUDE 'latc8m.f'
      INCLUDE 'hatc8m.f'
      INCLUDE 'porbc8m.f'   ! need for only OPERIOD
      INCLUDE 'unic8m.f'
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
C_End6789012345678901234567890123456789012345678901234567890123456789012_4567890

C 
      INTEGER*4 I,I1,I2,IRL,J,KREC
      INTEGER*4 NHF,NLF,NSF     ! number of hour/lats/seasons in far file
      REAL*8 DJONE              ! first date
      REAL*8 BUF(MAXN4)         ! fractional surface area in each latitude zone
      REAL*8 DELP(10+MAXN4)           ! type -3 file information
      REAL*8 DJ1F,DELJUF,FREC,FRAC,OMF,SEAPYR,DELSEAS
      REAL*8 FTS(MAXNH,MAXN4) !  fff Tsurf (hour,latitude]
      REAL*8 FTP(MAXNH,MAXN4) !  fff Tplan (hour,latitude] 
      REAL*8 FTA(MAXNH,MAXN4) !  fff Tatm  (hour,latitude]
      REAL SEASALB,SEASTAU      ! functions called
      REAL*4 ALB4,SUBS4,TAUD4  ! for *4*8 conversion 
      REAL*4 TIME1,TIME2,TIME3 
      LOGICAL LATM

      DELSEAS=.01                 ! tolerance on fractional seasonal offset
      LATM=PTOTAL.GT.1.         ! atmosphere present flag
      IRET=1                    ! normal
      IF (IDB1.NE.0) WRITE(IOSP,*) 'TSEASa',IQ,J5,LSC,N5,LONE

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

      IF (LOPN3) THEN           ! prepare for fff each season           !
        IF (LATM .AND. MINT .NE. 3) THEN ! If atmosphere, MINT is in /UNITS/
          IRET=41                ! then require fff atm, needed in TDAY
          WRITE(IOERR,*)'Case',NCASE,' Atm. needed in Far-field file'
          GOTO 9
        ENDIF
        CALL TFAR8(2,1,DELP, FTS,FTP,FTA) 
        IF (IDB1.EQ.3) WRITE(*,132)'DELP1:40',(DELP(I),I=1,40)  
 132    FORMAT(A10,/(10F8.3))
        NHF=INT(DELP(1))        ! number of hours in fff
        NLF=INT(DELP(2))        ! number of latitudes  in fff
        NSF=INT(DELP(3))        ! number of seasons in fff
        DJ1F=DELP(6)            ! first date in in fff
        DELJUF=DELP(7)          ! delta day in fff 
        IF (IDB3.GE.3) WRITE(IOSP,'(a,3i5,f9.3,f9.4)') 
     &       'TSEASb',NHF,NLF,NSF,DJ1F,DELJUL
      ENDIF
C  ----------------------------new season loop loop loop------------
C
 100  DJU5=DJONE+FLOAT(J5)*DELJUL ! current julian date
      J5=J5+1                   ! increment season index
      IF (LSC .OR. J5.EQ.IDOWN) THEN !  LSC  invokes changes at each season
        CALL TCARD8 (2,IRL)       ! read parameter change cards  
        IRET=IRL
        IF (IRL.GE.4) THEN
          WRITE (IOERR,*) 'TSEAS: TCard(2 return=',IRL 
          IF (IRL .GT. 5) IRET=10+IRL ! cannot continue this case
          GOTO 9             ! either 1-point or end-of-data
        ENDIF
D     write (*,*)'TSEAS: ',J5,N5,LOPN2,IDOWN,IRL !<< ,KVALB,alb
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
      SUBS4=SUBS                ! get R*4 version
      IF (KVALB .GT. 0) THEN
        ALB4=SEASALB(SUBS4)     ! variable soil albedo
        ALB=ALB4                ! move into R*8
      ENDIF
      IF (KVTAU .EQ. 1) THEN 
        TAUD4=SEASTAU(SUBS4)    ! variable atm. opacity
        TAUD=TAUD4
      ENDIF

Cvvvvvvvvvvvvvvvvvvvvvvvvvvv start of fff vvvvvvvvvvvvvvvvvvvvvvvvvvv
      IF (LOPN3 .AND. SLOPE .GT. 0) THEN ! fetch far field temperatures
C dj1f first date in the fff , deljuf=delta day in fff
        SEAPYR=OPERIOD/DELJUF ! seasons per year in the file
        FREC=DMOD(DJU5-DJ1F, OPERIOD)/DELJUF ! 0-based season index in fff
        IF (FREC .LT. 0.) FREC=FREC+SEAPYR ! advance one year
        FREC=FREC+1.D0          ! convert to 1-based season index
        I1=NINT(FREC)           ! nearest whole number
        FRAC=FREC-REAL(I1)      ! fraction way from nearest season 
        IF (ABS(FRAC) .lT. DELSEAS) THEN ! use single season
          I2=-2                 !  flag  indicating single season is OK
          OMF=1.
        ELSE                    ! need second season
          I1=INT(FREC)          ! round down
          FRAC=FREC-REAL(I1)    ! fraction way beyond lower season
          OMF=1.-FRAC           ! proportion of the lower season
          I2=I1+1               ! upper season
          IF (I2 .GT. NSF) I2=1 ! assume wrap to first season
        ENDIF
        IF (IDB3.GE.3) WRITE(IOSP,'(a,f8.4,2f9.3,f9.5,2i4)')
     &         'TSEASc',SEAPYR,DJU5,DJ1F,FRAC,I1,I2
        IF (I1 .GT. NSF) THEN 
          IRET=42                 ! signal an error
          GOTO 9
        ENDIF
        CALL TFAR8(3,I1+1,DELP, FTS,FTP,FTA) ! get season I1. +1 skips KRCCOM
        IF (DELP(3) .LE. 0.)  THEN 
          IRET=43                 ! signal an error
          GOTO 9
        ENDIF
        DO J=1,NLF              ! process first season, each latitude
          CALL MVDF (FTS(1,J),FARTS(1,J,1),NHF,OMF) ! fTs=omf*F3Fs
          IF (LATM) CALL MVDF (FTA(1,J),FARTS(1,J,2),NHF,OMF)! fTa=omf*F3Fa
        ENDDO  ! J
        IF (I2 .GT. 0) THEN     ! process second season, add fraction
          CALL TFAR8(3,I2+1,DELP, FTS,FTP,FTA) ! get season I2
          DO J=1,NLF
            CALL MVDA (FTS(1,J),FARTS(1,J,1),NHF,OMF) ! fTs=fTs+omf*F3Fs
            IF (LATM) CALL MVDA (FTA(1,J),FARTS(1,J,2),NHF,OMF) ! " for Ta
          ENDDO  ! J
        ENDIF                   ! I2 .GT. 0
      ENDIF                     ! LOPN3 .AND. SLOPE .GT. 0
C^^^^^^^^^^^^^^^^^^^^^^^^^^^ end of fff^^^^^^^^^^^^^^^^^^^^^^^^^^^

D       write (*,*)'TSEAS: ',J5,N5,LOPN2,IDOWN,SUBS ! ,KVALB,alb
C======

      CALL TLATS8 (IQ,IRL)       ! execute latitude loop

C======
      IF (IRL.NE.1) THEN
        WRITE(IOERR,*)' TSEAS:  TLAT error=',IRL 
        IRET=30+IRL             ! cannot continue this case
      ENDIF
C If there was a blowup, IRL will be 2  and will exit the season loop 
      IF (N4.GT.8 .AND. LATM) CALL TINT8 (FROST4, BUF, SUMF) !  MKS units  BUF = normalized area 
      IF (LP5) CALL TPRINT8 (5)  ! print latitude summary
      IF (LP6) CALL TPRINT8 (6)  !  & min and max layer temperature
C
C       write (*,*)'j5,jdisk,kold,k4out=',j5,jdisk,kold,k4out
C       write(*,*)'s1', subs
      IF (J5.EQ.JDISK .AND. KOLD.EQ.0 .AND. LOPN2 ! write  KRCCOM record
     &  .AND. K4OUT.LT.0) CALL TDISK8 (5,KREC) ! must follow tday(1)
      IF (J5.GE.JDISK) CALL TDISK8 (2,KREC) ! write this season
C
C Check if more seasons and no blowup
      IF (J5.LT.N5 .AND. IRL.EQ.1) GO TO 100
C  ---------------------------------------------------------------------
      IF (.NOT.LONE) THEN
        CALL CPU_TIME(TIME2)
        TIME3=TIME2-TIME1
        WRITE(IOPM,115) TIME3,'end',J5,N5
        WRITE(IOSP,115) TIME3,'end',J5,N5
      ENDIF  
      IRET=IRL
   
C     write(*,*)'s2', subs
9     RETURN
      END
