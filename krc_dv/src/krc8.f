      PROGRAM KRC
C_Titl  KRC planet surface thermal model  MGS-TES version
C_Vars
      INCLUDE 'krcc8m.f'      ! has IMPLICIT NONE
      INCLUDE 'latc8m.f'
      INCLUDE 'hatc8m.f'
      INCLUDE 'dayc8m.f'
      INCLUDE 'unic8m.f'
      INCLUDE 'filc8m.f'
C_Calls  CATIME  TCARD8  TDAY8  TDISK8  TPRINT8  TSEAS8 
C_Hist  85oct01  Hugh_H_Kieffer  Initial version circa 1969 at UCLA
C 87nov22  HHK  Send errors to screen, force parameter print if error.
C 93mar03  ECisneros Convert include filename to lowercase. Replace assign 
C   statement with an open statement, changed IOKEY variable from -4 to 5
C 93mar04  EC Removed excess code at end of program; add check for IEEE 
C   exceptions. previous version was running but giving IEEE exception.
C 97jan30  HHK Correct zero initialize
C 97feb11  HHK Revise commons and much alogorithm
C 97jul07  HHK From calories to  SI units
C 99dec07  HHK Add option to continue from current condition
C 2002mar07 HHK Add option for "one-point" rapid runs for Surface T
C 2006mar22 HHK Minor: change default input file to   krc.inp
C 2009mar05 HK Minor cleanup
C 2010jan11 HK Change to use of IMPLICIT NONE
C 2013jan16 HK Eliminate the multiple use of IR. Jan31: add version number 2.1.2
C 2013Feb16 HK Remove unused PORBCM
C 2013jul24 HK Start using new PORB system to fix Ls problems of v2.1.2 
C 2014jan27 HK If fatal error, try next case rather than terminating run.
C 2014feb26 HK Untabify and justify
C 2014mar10 HK Make  REAL*8  version    Change name from krc.f  v3.2.1
C 2015mar05 HK Add print of run-case separator to monitor. 
C 2015dec24 HK If TDAY(1) error will go to next case rather than quitting
C 2016feb12 HK Version 3.3.0; change in some commons, and some renamed
C 2016mar22 HK Version 3.3.1; minor glitch fixes. Add parameter print control
C 2016may14 HK Version 3.4.1: Allow multiple data files open. Far-flat field. 
C               Replace R2R using 2*N with D2D
C 2016aug12 HK Reset extreme numeric constants to Real*8 values. Use FILLMV set
C               Allow redirect of monitor output. Improve return codes
C 2016aug26 HK Automatically generate extensions for input and print files.
C 2016oct03 HK Correct sense of azimuth in tlats. Move onePoint heading print.
C 2017mar20 HK V 3.5.1 Incorporate eclipses and planetary heating
C 2017sep30 HK V 3.5.4 Error log name to the millisec to avoid failure on cluster
C 2018feb02 HK V 3.5.5 Separate Photometric function parameter from Henyey-Green.
C 2018jul03 HK V 3.5.6 Fixes to frost conditions. See 356notes.tex
C 2081oct11 HK V 3.6.1 Fixed ancient BINF5  bug. Checks on fff with atm. Put 
C  version into KRCCOM, Make far-field file REAL*4, backward compat. with REAL*8
C 2018nov13 HK v 3.6.2 Fix bug in 361 only; write of .t52 zeroed part of KRCCOM
C 2019mar20 HK Remove moot code to send all print to IOSP, this is done in TCARD
C 2020apr02 HK v 3.6.4 Changes are mostly cleanup in the PORB system
C 2020apr12 HK v 3.6.5 Lots of little cleanup trying to debug initial zone file
C_End6789012345678901234567890123456789012345678901234567890123456789012_4567890

      REAL ELAPSED,TIMES(2)     ! declare the types of DTIME()
      INTEGER I,IQ, IKON,IRC,IRD,IRL,KONE,KREC
      INTEGER J,K,N             ! for the NOWHITE algorithm
      INTEGER IOST              ! returned by OPEN
      CHARACTER*80 CBUF     ! temporary use
      CHARACTER(LEN=40) CELOG        ! error log file, takes 23 characters
      INTEGER*4 IZERO/0/        ! zero 
      INTEGER*4 VALS(8)           ! dummy arg for date_and_time or TFAR info
      LOGICAL LQ            ! file exists, later as temporaary flag
      CHARACTER(LEN=12) CDATE,CTIME ! args for date_and_time
C      CHARACTER*1 BBUF          ! temporary use
      CHARACTER*25 SEPER  /' ----------------------- '/  ! case separation line
      REAL TOTIME               ! Total Elapsed Time
      REAL*8 DUM8               ! dummy
      REAL*8 DUMB  /772.d0/     ! dummy argument, should never change
      REAL*8 DUMC  /773.d0/     ! " " 
      REAL*8 ZERO /0.0D0/       ! double precision zero  

      VERSIN='KRCv3.6.5'        ! set version number   12 bytes in FILCOM
      KREC=84+20  ! number of bytes in TITLE +DAYTIM. Values from def. in KRCCOM
      IF (MOD(KREC,8).NE.0 .OR. MOD(N4KRC,2).NE.0) THEN !possible alignment issue
        WRITE(*,*)'BAD lengths',KREC,N4KRC
        STOP
      ENDIF
      print *,'-FILLD',N4KRC,NWKRC,NWLAT,NWDAY,NWHAT
      I=MIN(N4KRC,NWKRC,NWLAT,NWDAY,NWHAT)
      IF (I.LT.1) STOP
C     Zero out commons, which contain some of the constants below
      CALL FILLD (ZERO,ALB, NWKRC )  !  KRCCOM includes R*8, int*4, logi*4, char
      CALL FILLD (ZERO,DTM4,NWLAT )  !  LATCOM
      CALL FILLD (ZERO,XCEN,NWDAY )  !  DAYCOM
      CALL FILLD (ZERO,FARTS,NWHAT)  !  HATCOM
      print *,'FILLD+',N4KRC,NWKRC,NWLAT,NWDAY,NWHAT

      FDISK='no'       !| ensure files turned off
      FDIRA='no'       !| by making their length
      FFAR ='no'       !| less than 4
      FFATM='no'       !|
C               set logical units. See   units.com   for description
      IOD1 = 1         ! explanation file & zone table (briefly open)
      IOD2 = 2         ! direct-access write (and read)  FDIRA  : 
      IOIN = 3         ! input file, defined in units.inc FINPUT : 
      IOKEY= 5         ! keyboard:    " "
      IOPM = 6         ! moniter:     " " May be redirected by IDB6>14
      IOSP = 7         ! print file : " "    FOUT
      IOERR = 9        ! error log:   " " May be redirected by IDB6>14 CELOG
      IOD3 = 13        ! direct-access far-field, read only
      IODA = 14        ! direct-access far-field, atm, read only
      LOPN2 =.FALSE.   ! IOD2: FDIRA set to: no direct-access write open
      LOPN3 =.FALSE.   ! IOD3: FFAR  set to: no far-field active
      LFATM =.FALSE.   ! IODA: FFATM set to: no far-field atmosphere active
      LOPN4 =.FALSE.   ! ----: FDISK set to: no Type 52 active
      LONE  =.FALSE.   ! set to: Not one-point mode
C                       set constants
      I=LEN_TRIM(VERSIN)        ! and make decimal interger
      WRITE(CBUF,*) VERSIN(1:I) ! write string into temporary buffer
C        write(iopm,*)'>',cbuf,'<'
C        write(iopm,*)'>'//cbuf//'<'
      READ (CBUF,'(5x,I1,1x,I1,1x,I1)'),I,IQ,KONE ! read as 3 digits
C        write(iopm,*)I,IQ,KONE
      NUMVER=100*I+10*IQ+KONE   ! Version number as integer, into common
C        write(iopm,*)'numver',numver
      PIVAL = 3.14159265D0      ! pi
      RADC = 180.D0/PIVAL       ! degrees/radian
      SIGSB = 5.670367D-8 ! (13) Stephan-Boltzman constant:  W m^-2 K^-4 2018oct
      RGAS = 8.3144598D0    ! (48) molar gas constant  (MKS=J/mol/K) 2018oct
C in Fortran 90, the following 3 could use intrinsic functions 
C      HUGE = 3.3D38             ! largest   REAL*4 constant  F90 HUGE
C      TINY = 2.0D-38            ! smallest  REAL*4 constant  F90 TINY
C      EXPMIN = 86.80D0          ! neg exponent that would almost cause underflow
      HUGE = 1.D308             ! large  REAL*8 constant with margin
      TINY = 1.D-307            ! small  REAL*8 constant with margin
      EXPMIN = 700.d0           ! safe negative exponent yields ~1.d-304 
      KREC=0                    ! ensure it has a storage location
      NRUN=0                    ! no output file yet
      NCASE=0                   ! initiate case counter
      WRITE(IOPM,*) 'This is KRC:  ',VERSIN,NUMVER
C 2017sep30 replace error-file name to nearest min with name to nearest millisec
C- Lines are to redirect error to monitor, but IDB4 not yet known!
      CALL CATIME (CBUF)
      DAYTIM=CBUF(2:21)
C-      IF (MODULO(IDB4,2) .EQ. 1) THEN ! no error file 
C-         IOERR=IOPM              ! send error to monitor, no error file
C-       ELSE                      ! create error file
C      CBUF=DAYTIM(10:11)//DAYTIM(6:8)//DAYTIM(13:14)//DAYTIM(16:17) ! ddMONhhmm
C      BBUF=CBUF(1:1)
C     IF (BBUF.EQ.' ') CBUF(1:1)='0' ! avoid a blank in the name for days 1:9
      LQ=.TRUE. ! Following delay loop in case many runs on cluster
      IQ=-1
C Following section in case of many simultaneous runs on a cluster
      DO WHILE (LQ .EQV. .TRUE.) ! delay loop to ensure unique file name
        CALL DATE_AND_TIME(CDATE,CTIME,CBUF,VALS) !  3rd arg  is not needed
        IQ=IQ+1 ! how many loops of delay used
        I=LEN_TRIM(CDATE)
        J=LEN_TRIM(CTIME)
        CELOG='eLog'//CDATE(1:I)//'T'//CTIME(1:J) ! error file name
        N=LEN_TRIM(CELOG)
C        print *,'initial',celog
C        DO I=1,N  ! Replace any non alpha-numeric with zero
C          BBUF=CELOG(I:I)         ! single character
C          J = ICHAR (BBUF)
C          IF (J.LT.48 .OR. J.GT.122 .OR. (J.GT.90 .AND. J.LT.97) ) 
C     &         CELOG(I:I)='0'
C        ENDDO   
C        print *,'IQ,CELOG',IQ,CELOG
        INQUIRE (FILE=CELOG(1:N),EXIST=LQ) ! does such a file already exist?
      ENDDO

C test of calling NOWHITE. 2020apr12 compile with -fbounds-check causes failure
C        print *, 'N,celog=',N,celog
C        CALL NOWHITE(C80, I,CELOG) ! retain only printable characters
C        print *, 'I,celog=',I,celog
C end of test

      WRITE(IOPM,*)'DAYTIM = ',DAYTIM,IQ,'=IQ   errorfile = ',CELOG 
      OPEN (UNIT=IOERR,FILE=CELOG(1:N),STATUS='NEW',err=85) ! quit if open fails
C                       open input and print files 
      WRITE (IOPM,*)' .inp and .prt will be added to your input names'
      WRITE (IOPM,*)' Defaults:  input = krc , output = input' 

 50   CBUF = 'krc'              ! default input file name
      ID24=8                    ! reset DA word size to R*8
      WRITE (IOPM,*)'?* Input file name or / for default =',CBUF(1:3)
      READ (IOKEY,*,ERR=50,end=9) CBUF
      IQ = LNBLNK(CBUF)         ! location of last non-blank character 
      FINPUT =CBUF(1:IQ)//'.inp' ! add extension 
      OPEN (UNIT=IOIN,FILE=FINPUT,STATUS='OLD',err=81)

 60   WRITE (IOPM,*)'?* Print file name or / for default =',CBUF(1:IQ)
      READ (IOKEY,*,ERR=50,end=9) CBUF
      IQ = LNBLNK(CBUF)
      FRUN=CBUF(1:IQ)           ! save as the run name 
      FOUT=CBUF(1:IQ)//'.prt'   ! add extension 
      OPEN (UNIT=IOSP,FILE=FOUT,err=82)
C                       read and check a complete set of input parameters
D     write(iosp,*) 'before TCARD LP2=',LP2 !<<< debug
      CALL TCARD8 (1,IRC)
D     write(iosp,*) 'after TCARD IR,LP2=',IRC, LP2 !<<< debug
D     write(*,*) 'TCARD:1 return=',IRC !<<< debug
      IF (IRC.GT.4) GO TO 180 ! end-of-data or error
      CALL DTIME(TIMES,ELAPSED) ! Start clock, GNU recommended form 
      TOTIME=0.
      IRD=7  ! This will force print of parameters for the first case

C VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV  BEGIN case  VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV
 140  NCASE=NCASE+1 ! increment case
      LATM=(PTOTAL .GT. 1.)     ! case has an atmosphere, into KRCCOM
      PARC(12)=PERIOD           ! sol in days for an eclipsed body
      IF (IRC.EQ.4) THEN        ! Switch to "one-point" mode
        CLOSE(IOIN)             ! close the card input file
D       write(*,*)'FINPUT=',finput !<<< debug
        OPEN (UNIT=IOIN,FILE=FINPUT,STATUS='OLD',iostat=iost,err=81)
D       write(*,*)' IOSTAT=',iost !<<< debug
        READ (IOIN,'(A80)',ERR=83,END=84) CBUF ! read Users title
D       write(*,*)' k2' !<<< debug
        WRITE(IOSP,*)'---- Start of one-point mode ----'
        WRITE(IOSP,*)CBUF       ! write users title
        READ (IOIN,'(A80)',ERR=83,END=84) CBUF ! skip the col header line
D       write(*,*)' k3' !<<< debug
        LONE=.TRUE.
        KONE=NCASE              ! case when onePoint started
        CALL TCARD8 (2,IRC)     ! read first one-point case
D       write(IOSP,*) 'KRC TCARD:2 return=',IRC !<<< debug
      ENDIF 

D     write(IOSP,*)'KRC TDAY 1 return, LP2',IRD,LP2 !<<< debug
      I=1
      IF ((LP2 .AND. LD18) .OR. (IRD.NE.1)) THEN
        CALL TPRINT8 (2)        ! print input parameters
        I=3                     ! this will print layer table
      ENDIF
      IF (PARC(1).GT.0. .OR. PARW(1).GT.0. ) 
     & WRITE(IOSP,131) 'Eclipse or PlanHeat on',PARC(1),PARW(1)
 131  FORMAT(1X,A,2F10.3)

      CALL TDAY8 (I,IRD)   ! initialize day computations. Can print layer table

      IF (I.EQ.3) LD18=.FALSE.  ! clear the "something changed" flag
C Above changes some items used in TPRINT8 (2) 2016feb NOPE ?
      IF (IRD.NE.1) THEN
        WRITE(IOSP,*)' PARAMETER ERROR IN TDAY(1) ', IRD
        IRL=4                   ! Fake error return from TSEAS
        J5=JDISK                ! fake being at first output season
        CALL FILLL (IZERO,NDJ4,MAXN4E)  ! set all iteration days to 0 as a flag
        GOTO 170                ! write error messages, then try next case
      ENDIF
      IKON = IRC                ! transfer "continuing" flag from  TCARD to  TSEAS
      IF (LONE) IKON=1          ! set TSEAS to start fresh

      IF (N5.GE.JDISK) THEN     ! there may be some file output 
D       WRITE(IOPM,*)'L3',LOPN2,LOPN3,LOPN4   !?Dbug
        I=LNBLNK(FDISK)
D       WRITE(IOPM,*)'FDISK,I ',FDISK(1:I),I !?Dbug
        IF (.NOT.LOPN4 .AND. I.GE.4) THEN
          CALL TDISK8 (6,0)     ! open output type 5x
          NRUN=NRUN+1           ! increment run count
          NCASE=1               ! restart the case count, used by TDISK
          IF (.NOT.LOPN4) WRITE(IOSP,*)'ERROR, Case too big  for KOMMON'
        ENDIF
        I=LNBLNK(FDIRA)
D       WRITE(IOPM,*)'FDIRA,I ',FDIRA(1:I),I !?Dbug
        IF (.NOT.LOPN2 .AND. I.GE.4) CALL TDISK8 (1,0) ! open output DirAcc file 
        IF (I15.GT.100) CALL TUN8 (I15,1) ! Write case header 
D       WRITE(IOPM,*)'LOPN2,3,4= ',LOPN2,LOPN3,LOPN4   !?Dbug
      ENDIF
D     write(*,*) 'cond=',cond !<<< debug
D     write(*,*)'KRC LP4=',LP4 !<<< debug
C     CALL CATIME (DAYTIM)      ! reset the time at start of each model

C - - - OPEN fff input surf [and atm]
      DUM8=MODULO (IDB6,4)      ! Set amount of reporting 
      LQ=(IDB6 .NE. 0)          ! local flag for fff debug print
      IF (LQ) WRITE(IOPM,*) 'KRC:4L',LATM,LFATM,MINT,LFAME,LOPN3  !<<<< debug
      I= LNBLNK(FFAR)           ! length of far-field surface name
      IF (LQ) WRITE(IOPM,*)'KRC:FFAR>',FFAR(1:I),'<',I  !<<<< debug
      IF (I.GE.4 .AND. .NOT.LOPN3) THEN
        CALL TFAR8(1,0,DUM8,VALS, DUM8M,DUMB,DUMC) ! open far-field
C May omit last 2 arguments ^^, but then debugger cannot access in subroutine
        IF (LQ) WRITE(IOPM,*)'KRC:sMEMI:',VALS
        IF (LQ) WRITE(IOPM,'(A,59F8.2)' )'KRC:sFELP:',DUM8M(1:10+N4)
C    Success mandatory to prevent possible long run with wrong fff
        IF (.NOT. LOPN3 .OR. DUM8 .LT. 0) THEN   !  err msg written by TFAR8
          WRITE(IOERR,*)'Case',NCASE,' FarFieldFile did not open? ',FFAR
          GOTO 9
        ENDIF
        IQ=VALS(1)              ! N24 in the fff
        IF (LOPN3 .AND. N2 .GT. MAXFF) THEN
          WRITE(IOSP,*)'WARNING: fff use, Reducing N2 from,to',N2,MAXFF
          N2=MAXFF
        ENDIF
      ENDIF                     ! opening FFAR 
      I= LNBLNK(FFATM)          ! length of far-field atmosphere name
      IF (LQ) WRITE(IOPM,*)'KRC:FFATM>',FFATM(1:I),'<',I   !<<<< debug
      IF (I.GE.4 .AND. .NOT.LFATM) THEN
        CALL TFAR8(11,0,DUM8,VALS, DUM8M,DUMB,DUMC) ! open far-field
C    Success mandatory to prevent possible long run with wrong fff
        IF (LQ) WRITE(IOPM,*)'KRC:aMEMI:',VALS
        IF (LQ) WRITE(IOPM,*)'KRC:aFELP:',DUM8M(1:10+MAXN4)
        IF (.NOT. LFATM .OR. DUM8 .LT. 0) THEN   ! error msg written by TFAR8
          WRITE(IOERR,*)'Case',NCASE,'FarFieldFile did not open?',FFATM
          GOTO 9
        ENDIF
        IF (VALS(1).NE.IQ) THEN ! N24 different, interpolation in TLATS would fail
          WRITE(IOERR,*)'Case',NCASE,'fff Tsurf and Tatm N24 different'
          GOTO 9 ! next case would probably try to open the same file
        ENDIF
        IF (VALS(4) .NE. 3)  GOTO 86 ! file was expected to provide atm. temp.
        IF (DUM8M(8).NE. 0.) WRITE(IOSP,*) ' Warning: far Tatm sloped'
      ENDIF                     ! opening FFATM

      IF (LQ) WRITE(IOPM,*) 'KRC:5L',LATM,LFATM,MINT,LFAME,LOPN3 
      LFAME=(LATM .AND. .NOT. LFATM .AND. MINT .EQ. 3) ! get atm from surface fff
      IF (LATM .AND. LOPN3 .AND. .NOT.LFATM .AND. .NOT.LFAME) GOTO 86 ! no fff atm

C======

      WRITE(IOSP,'(a,a,2I6,a)')SEPER,' RUN-CASE',NRUN,NCASE,SEPER
      CALL TSEAS8 (IKON,IRL)       ! %%%%% execute season loop %%%%

C======
C If TSEAS called TCARD with return 1:5 then IRL will have that value, 
C      and action will fall out the bottom of the season loop
C      If failure >5, then +10, terminate case 
C If TDAY  had failure >1, then IRL will be +20, terminate case 
C If TLATS had failure >1, then IRL will be 31:39, terminate case
C For details, see code or Helplist section 'Error Returns' 
D      write(*,*)'TSEAS return IKON,IRL,N5,krec=',IKON,IRL,N5,krec !<<< debug

      IF (LONE) THEN
        IF (NCASE.EQ.KONE) WRITE(IOSP,'(A,A)')'C_END  Ls   Lat  Hour '
     & ,'Elev  Alb Inerti Opac Slop Azim  TkSur  TbPla Comment' 
        CALL TPRINT8(9)         ! print results at requested one-point
      ENDIF
      
      IF (IRL.EQ.3) NCASE=NCASE-1 ! case stays the same when continue from memory
      IF (IRL.GT.4 .AND. N5.GT.0) THEN ! Case had a fatal error
        WRITE(IOPM,*)'Case had FATAL error=',IRL,' Will try next case'
        WRITE(IOSP,*)'Case had FATAL error=',IRL,' Will try next case'
        IF (LD18) THEN          ! print the input conditions
          CALL TPRINT8 (2)      ! print input parameters
          CALL TDAY8 (3,IRD)    ! print layer table 
          LD18=.FALSE.          ! clear the "something changed" flag
        ENDIF  
        IF (IRL.EQ.5) GOTO 180  ! out of change cards, must stop    
      ENDIF
      IF (LOPN2) CALL TDISK8 (4,KREC) ! close FDIRA, cannot hold multiple cases
      FDIRA='NO'                ! ensure it stays closed ( length < 3 )
C
 170  IF (.NOT. LONE) THEN      ! 
        CALL DTIME(TIMES,ELAPSED) ! elapsed seconds
 133    FORMAT(1X,'Case',i3,2x,a1,'TIME: total, user, system=',3f10.4)
        WRITE(   *,133)NCASE,'D', ELAPSED,TIMES ! always to monitor
        WRITE(IOPM,133)NCASE,'D', ELAPSED,TIMES ! may go to a file
        WRITE(IOSP,133)NCASE,'D', ELAPSED,TIMES ! to the print file
        TOTIME=TOTIME+ELAPSED   ! increment total time
      ENDIF
      CALL TCARD8 (2,IRC)       ! read set of parameter changes
C
D     WRITE(IOSP,*)'TCARD:2 IR=',IRC,krec  !<<< Debug
      IF (IRC.LT.5) GOTO 140    ! 5 is END of data
C AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA end case AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
C
 180  Write(IOSP,*)'KRC@180 IRL,IRC=',IRL,IRC
      IF (LOPN2) CALL TDISK8 (4,KREC) ! all done: close direct acess output file
      IF (LOPN3) CALL TFAR8 ( 4,DUM8,DUM8,VALS,DUM8M,DUMB,DUMC) ! far-field surf
      IF (LFATM) CALL TFAR8 (14,DUM8,DUM8,VALS,DUM8M,DUMB,DUMC) ! far-field atm
      IF (LOPN4) CALL TDISK8 (7,KREC) ! write type 5x file
 134  FORMAT(6X,'END KRC   Total time [s]=',F11.3 )
      WRITE(IOPM,*)'DUMB,C=',DUMB,DUMC !<< debug
      WRITE (   *,134)TOTIME
 9    WRITE (IOSP,134)TOTIME
      WRITE (*,*)'Elog= ',CELOG
      WRITE (IOSP,*)'Elog= ',CELOG
      STOP

C error section
 81   WRITE(IOERR,*)'KRC: error opening input file =',FINPUT,IOST
      GOTO 50                   ! another input file
 82   WRITE(IOERR,*)'KRC: error opening print file =',FOUT
      GOTO 60                   ! another print file
 83   WRITE(IOERR,*)'KRC: error reading one-point header lines',FDISK 
      GOTO 9
 84   WRITE(IOERR,*)'KRC: unexpected EOF reading one-point header' 
      GOTO 9
 85   WRITE(IOPM,*)'KRC: failed opening error log file'  
      GOTO 9
 86   WRITE(IOERR,*)'KRC: need fff atmosphere but none available',NCASE
      GOTO 170                  ! try next case

      END
