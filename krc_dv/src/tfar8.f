      SUBROUTINE TFAR8 (KODIN,FFATE,FFOLE, MEMI, FFTS,FFTP,FFTA)
C_Titl  TFAR8  Open and read KRC type -N direct-access files, 1 or 2 in parallel
C_Vars
      INCLUDE 'krcc8m.f'        ! has IMPLICIT NONE
C Here refer to:  DAYTIM  FD  ID  KFARAC  LD  MAXN4  NUMFD  NUMH4  NUMID  NUMLD 
C     INCLUDE 'latc8m.f'
C     INCLUDE 'dayc8m.f'
C     INCLUDE 'hatc8m.f'
      INCLUDE 'unic8m.f'  ! contains MINT,  IOD3,LOPN3,  IODA,LFATM  LFAME IDBx
      INCLUDE 'filc8m.f' ! has FFAR, FFATM
      INTEGER NFELP
      PARAMETER (NFELP = 10+MAXN4) ! size of real info storage
C_Args
      INTEGER*4 KODIN            ! in. control current KRCCOM 
C 0:4 is for Tsurf, using IOD3, LOPN3, FFAR
C        1 = Open file .  FFATE: +1=check KRCCOM  +2=stat file
C          will set  LOPN3=.true. if open successful
C        2 = Return help about the file in  FFTS
C        3 = Read seasons bounding FFATE and interpolate [if neeeded].
C        4 = Close the file.  FFATE ignored
C  +10 is for Tatm, using IODA, LFATM, FFATM
      REAL*8 FFATE     !in  KRC date requested. Ignored unless KODIN=3[+10}
      REAL*8 FFOLE     !in. Fraction tolerance to use nearest single season.
C if KODIN=1, Controls reporting. +1=compare parameters +2=Print file status
          ! out. If negative, some fatal error occured. -3 more for 2nd file
               ! -1 or -4 request for file information, but none open
               ! -2 or -5 invalid number of temperature types (records per season)
               ! -3 or -6 an I/O error occured in TFAREAD
      INTEGER*4 MEMI(8) ! out: information about file
C  1: Number of hours defined: N24    2: Number of latitudes defined  
C  3: Number of seasons in file   If returned 0, (kode=2), file not found
C         If returned negative  (kode=3), item 4 (# T sets) is not what's needed
C  4: Number of temperature sets in file
C  5: seasons/year in file       6: Number of records in file
C  7: file KRC version number    8: Flag: 4= file is R*4, else R*8 
      REAL*8 FFTS(NUMH4) ! out. Tsurf (hour,latitude]  Ignored if KOD4 is 4 
C If KODE is 1 or 2, will be file information FFELP(1:NFELP) containing:
C   1: spare    2: date of first season    3: delta day between seasons     
C   4: Last date in file                   5: year length in days        
C   6: number of records in file           7: emissivity
C   8: slope    9:10: spare              11+: latitude values
      REAL*8 FFTP(NUMH4) ! out. Tplan (hour,latitude]  Used only if KODE is 3 
      REAL*8 FFTA(NUMH4) ! out. Tatm  (hour,latitude]  Used only if KODE is 3 
C_Lims
C Must not modify any values in krcc8m.f
C_Calls   MV4D 
C_Desc
C  Can read KRC type -1,- 2, -3 files. See helplist.txt
C  Initially assumes type -1, then checks K4OUT in file and can retry
C_Hist 2016may11 Hugh Kieffer Adopt from tdisk8.f
C 2016may21 HK Enable reading files with 1,2, or 3 kinds of temperature
C 2018oct12 HK Version 361.  Files may be REAL*4
C 2018nov05 HK Prepend D to lines activated by IDBx
C 2019mar20 HK Add estimate for days/year for versions before 361
C_Pause
C Make arrays that match COMKRC(NWKRC)   Definitions MUST MATCH KRCCOM exactly
      REAL*8 FF3FD(NUMFD)        ! Real parameters
      REAL*8 FF3LA(MAXN4)        ! latitude in degrees. | N4 defined
      REAL*8 FF3EV(MAXN4)        ! elevation in km.     | N4=FF3ID(4)
      INTEGER*4 FF3ID(NUMID)     ! Integer parameters
      LOGICAL*4 FF3LD(NUMLD)     ! Logical parameters
      CHARACTER*80 FILEIN       ! fff file
      CHARACTER*84 FF3KIT        ! purpose line from the input file
      CHARACTER*20 FF3DAY        ! date/time that file was created
C   COMMON /F3KCOM/ FF3FD,FF3LA,FF3EV,FF3ID,FF3LD,FF3KIT,FF3DAY  ! to match KRCCOM
C To handle 1 or 2 or 3  T-sets
C Note: Direct write zero-fills unused part of record

      REAL*8 FFELP(NFELP) ! Real values from file KRCCOM, current
      REAL*8 FFEL1(NFELP) ! " ", file 1
      REAL*8 FFEL2(NFELP) ! " ", file 2
      INTEGER*4 MEM1(8),MEM2(8) ! memory of integer values from file KRCCOM
      REAL*8 XF,XG,XH           ! reusable
      REAL*8 FFRAC              ! fraction of 2nd season
      INTEGER*4 KODE
      INTEGER*4 KACT            ! reporting action than can be changed
      INTEGER*4 I,II,ILEN,IOP,IOST,IFUN,JS1,JS2,KR1,KR2
      INTEGER*4 MREC,NWTOT,NRECL
      INTEGER*4 IR1,IR2,IR3     ! return codes
      INTEGER*4 BUFF(13)        ! for file status
      LOGICAL*4 LISS            ! .tmx file expected to be single precision
      LOGICAL*4 LOPF            ! status of current fff logical unit

      SAVE FFEL1,FFEL2,MEM1,MEM2 ! insure these remembered between calls
C     
D     IF (IDB6.GE.2) WRITE(IOSP,*)'TFAR:0 ',KODIN,FFATE,FFOLE
      KODE=MODULO(KODIN, 10)
      IF (KODIN .LT. 10) THEN  ! set to file 1 = for Tsurf ( +Tatm)
        FILEIN=FFAR             ! file name
        IFUN=IOD3               ! logical unit to use
        LOPF=LOPN3              ! logical unit status
        CALL MVL(MEM1,MEMI,8)   ! get proper integer info
        CALL MVD(FFEL1,FFELP,NFELP) ! get proper R*8 info
      ELSE                      ! set to file 2 = for Tatm
        FILEIN=FFATM
        IFUN=IODA
        LOPF=LFATM
        CALL MVL(MEM2,MEMI,8)
        CALL MVD(FFEL2,FFELP,NFELP)
      ENDIF
      ILEN = LNBLNK(FILEIN)     ! how long is the file name
      LISS=(MEMI(8).EQ.4)       ! true if file is R*4
D     IF (IDB6.GE.2) WRITE(IOSP,*)'TFARfile: ',FILEIN(1:ILEN),ILEN
      SELECT CASE(KODE)   !vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv 
   
      CASE(1)                   ! open file            1  1  1  1  1  1  1  1
        IF (LOPF) CLOSE (IFUN)
        LOPF=.FALSE.
        KACT=NINT(FFOLE)        ! Action kode
C     recl may be bytes or longwords, depending upon  OS and compiler options.
        NWTOT=NUMH4       ! each of TSFF TPFF and TATM
        NRECL=8*NWTOT    ! bytes: or  NRECL=NWTOT  ! depends upon compiler <<<<
        I=1                     ! Assume file will be default R*8
 50     OPEN (UNIT=IFUN,FILE=FILEIN,ACCESS='DIRECT',STATUS='OLD'
     &       ,RECL=NRECL,ERR=191,IOSTAT=IOST)
        WRITE(IOSP,110)FILEIN(1:ILEN),NWTOT, NRECL,DAYTIM
 110    FORMAT (/'TFAR: Opened direct access file = ',A
     &       /' NWTOT & NRECL used=',2I7,'  Now=',A)
        II=1 ! record containing KRCCOM
        READ (IFUN,REC=II,IOSTAT=IOST) FF3FD,FF3LA,FF3EV,FF3ID,FF3LD
     &       ,FF3KIT,FF3DAY  ! Order  MUST MATCH KRCCOM exactly
        IF (I .EQ. 1 .AND. FF3ID(24) .EQ. 4) THEN  ! reopen as R*4
          CLOSE(IFUN)           ! close the file
          I=2                   ! do at most once
          NRECL=4*NWTOT         ! set for R*4 seasonal records
          GOTO 50
        ENDIF
        LOPF=.TRUE.             !
        CALL FSTAT(IFUN, BUFF)  ! get file status
        FFELP(6)= DBLE(BUFF(8))/DBLE(NRECL) ! number of records it contains
        MREC=NINT(FFELP(6))     ! nearest integer
        XF= FFELP(6)-MREC       ! check on NRECL; should be an integer
        IF (XF .NE. 0.D0) WRITE(IOERR,*)'TFAR: NRECL excess=',XF
        MEMI(1)=FF3ID(6)        ! Number of hours defined
        MEMI(2)=FF3ID(4)        ! number of latitudes defined
        MEMI(4)= -FF3ID(17) ! negative of K4OUT in the file = Number of T sets
        MEMI(3)=(MREC-1)/MEMI(4) ! Number of seasons in file
        XG=FF3FD(68)            ! days/year FD(68). Was spare=0 until v361
        XH=FF3FD(42)            ! deljul
        IF (XG.LT.XH) THEN  ! probably before v361
          XG=(MEMI(3)-1)*XH     ! Assume recorded a year with 1 season wrap
          WRITE(IOSP,*)'WARNING, days/year uncertain. Set to', XG
        ENDIF
        XF=XG/XH                ! seasons/year in file
        MEMI(5)=NINT(XF)        ! " as integer
        MEMI(6)= MREC           ! number of records in file
        MEMI(7)= FF3ID(25)      ! NUMVER:   prior to 3.6.1 files were REAL*8
        MEMI(8)= FF3ID(24)      ! Bytes per word, defined for v361 at later
        IF (ABS(XF-MEMI(5)).GT. 1.D-2 ) ! FIRM-CODE tolerance
     &         WRITE(IOERR,*)'TFAR: non-I seasons=',XF
        FFELP(2)=FF3FD(87)-(MEMI(3)-1)*XH ! DJUL of first season in file
        FFELP(3)=XH      ! FF3FD(42)  DELJUL between seasons
        FFELP(4)=FF3FD(87)      ! DJUL of last season in file
        FFELP(5)=XG     ! FF3FD(68) ?. Length of a orbital year in days
        FFELP(7)=FF3FD(2)       ! surface emissivity
        FFELP(8)=FF3FD(23)      ! slope
        CALL MVD (FF3LA,FFELP(11),MEMI(2)) ! transfer latitudes
        IF ( (MEMI(1) .NE. N24) .OR. (MEMI(2) .NE. N4)) THEN ! bad sizes
          WRITE (IOERR,*)'TFAR: N hour|lats disagree',N24,N4,MEMI(1:2)
          LOPF=.FALSE.          ! will alert KRC of a failure
          CLOSE(IFUN)           ! close the file
          KACT=3                ! force full reporting
        ENDIF
        IF (MOD(KACT,2).EQ.1) THEN !  compare parameters
          WRITE(IOSP,*)'TFAR: KRCCOM Comparison:'
          DO I=1,NUMFD
            XF=ABS(FD(I))       ! steps to get normalized difference
            XG=ABS(FF3FD(I))
            XH=MAX(XF,XG)
            IF (XH .NE.0.) THEN   
              XF=(FD(I)-FF3FD(I))/XH ! normalized change
              IF (XF .NE. 0.) 
     &             WRITE(IOSP,*)'FD diff:',I,FD(I),FF3FD(I),XF
            ENDIF
          ENDDO   
          WRITE(IOSP,*)'Latitudes and elevations'
          WRITE(IOSP,'(10F8.2)')(FF3LA(I),I=1,MEMI(2)) 
          WRITE(IOSP,'(10F8.2)')(FF3EV(I),I=1,MEMI(2))
          DO I=1,NUMID
           IF (FF3ID(I).NE.ID(I)) 
     &         WRITE(IOSP,*)'ID diff:',I,ID(I),FF3ID(I)
          ENDDO
          DO I=1,NUMLD
            IF (FF3LD(I).NEQV.LD(I))
     &          WRITE(IOSP,*)'LD diff:',I,LD(I),FF3LD(I)
          ENDDO
        ENDIF
        IF (KACT.GE.2) THEN     ! print file status
          WRITE (IOPM,FMT="('TFAR: File size:',    T30,I19)") BUFF(8)
          WRITE (IOPM,FMT="(' Last modify. time ',A19)")CTIME(BUFF(10))
          WRITE (IOPM,FMT="(' Preferred block size:',T30,I19)") BUFF(12)
          WRITE (IOPM,FMT="(' No. blocks allocated:',T30,I19)") BUFF(13)
          WRITE (IOPM,*)'MEMI=',MEMI
        ENDIF
        IF (KODIN .LT. 10) THEN   ! store Int and Real Information
          MINT=MEMI(4)            ! set only for surface fff.  Atm fff must be 3
          CALL MVL(MEMI,MEM1,8)   ! store for file 1
          CALL MVD(FFELP,FFEL1,NFELP) ! " " 
        ELSE
          CALL MVL(MEMI,MEM2,8)   ! store for file 2
          CALL MVD(FFELP,FFEL2,NFELP) ! " " 
          IF (MEMI(4) .NE. 3) FFOLE=-2. ! Error, must have T-atmosphere
        ENDIF
        GOTO 7

      CASE(2)          ! only provide help info        2  2  2  2  2  2  2  2
        IF (LOPF) GOTO 7       ! just return help
        FFOLE=-1.              ! Flag as no data available
        WRITE (IOERR,*) 'TFAR:2, file is not open ',FILEIN
C     
      CASE(3)         ! read requested season          3  3  3  3  3  3  3  3  3
        IF (LOPF) THEN
          II=MEMI(4)          ! Number of temperature sets in file
          IF (II.LT.1 .OR. II.GT.3) THEN  ! somewhere Mint has been changed 
            WRITE (IOERR,*)'TFAR:3 Invalid Mint=',II
            FFOLE=-2. ! Fatal, could transfer out-of-bounds 
          ELSE
C                WRAPER8 (   DR,   D1,    DELD,    NUMD,   DPER,   FTOL
            CALL WRAPER8 (FFATE,FFELP(2),FFELP(3),MEMI(3),FFELP(5),FFOLE
     & ,  JS1,FFRAC,JS2,IR1)
C        , J1,FX,    J2,IRET)
            IF (MODULO(IR1,2) .EQ. 1) THEN
              XG= FFELP(5)/FFELP(3)
              WRITE(IOERR,*)'TFAR: Year/DELJUL not integral',XG
            ENDIF
            IF (IR1 .GE. 2) WRITE(IOERR,*)
     &         'TFAR: Season outside fff; date=',IR1,FFATE
            IR1=0             ! set return codes clear in case not used
            IR2=0
            IR3=0
            IOP=IOPM            ! transfer print unit
            KR1=2+(JS1-1)*II  ! record number of Tsurf for season JS
            KR2=2+(JS2-1)*II  ! = krccom+ n*(seasons to disk already)+1
            IF (KR1.LT.1) THEN 
              WRITE (IOERR,'(A,6F10.3)') 'TFAR8 fault',FFATE
     &             ,(FFELP(I),I=1,5)
              WRITE (IOERR,'(A,F10.4,I6,F10.4,2I6)') 'TFAR8  wrap' 
     &             ,FFOLE,JS1,FFRAC,JS2,IR1
              WRITE (IOERR,'(A,8I6)') 'TFAR8  MEMI',(MEMI(I),I=1,8)
              ENDIF
            CALL TFAREAD (IFUN,KR1,FFRAC,KR2,NUMH4,LISS,IOP, FFTS,IR1)
C                        (IOD3,KR1,FFRAC,KR2,NUMH4,LISS,IOP, AAA,IRET)
            IF (II.GE.2) THEN ! next record is Tplan
              KR1=KR1+1
              KR2=KR2+1
              CALL TFAREAD (IFUN,KR1,FFRAC,KR2,NUMH4,LISS,IOP, FFTP,IR2)
            ENDIF
            IF (II.EQ.3) THEN ! next record is Tatm
              KR1=KR1+1
              KR2=KR2+1
              CALL TFAREAD (IFUN,KR1,FFRAC,KR2,NUMH4,LISS,IOP, FFTA,IR3)
            ENDIF 
          ENDIF                 !  MEMI(4) is  OK
          I=IR1+IR2+IR3 ! all return codes
          IF (I .NE. 0) THEN 
            WRITE (IOERR,*)'TFAR READ:  returns codes;',IR1,IR2,IR3
            FFOLE=-3.            ! set error return
          ENDIF
          IF (KFARAC.EQ.1) WRITE(IOSP,310)FILEIN(1:ILEN),JS1
 310      FORMAT(' TFAR: READ FILE= ',A,' Season 1=',I4)
        ELSE ! LOPN3
          WRITE (IOERR,*)'TFAR:3, READ, BUT unit OPEN',IFUN
        ENDIF
C     
      CASE(4)                   !  close the file     4  4  4  4  4  4  4  4
        IF (LOPF) then
          CLOSE (IFUN)
          WRITE (IOSP,*)'Closed DA file. Name, Unit, -Type=',
     &         FILEIN(1:ILEN),IFUN,MEMI(4)
          LOPF=.FALSE.
          IF (KODIN .LT. 10) THEN ! set the file name off
            FFAR='off'
          ELSE
            FFATM='off'
          ENDIF    
        ELSE                    ! was not open
          WRITE (IOERR,*)'TFAR:4, no file was open for unit',IFUN
        ENDIF

      CASE DEFAULT                 ! else   else   else  else   else   else  
        WRITE (IOERR,*)'TFAR:5, Called with invalid KODIN=',KODIN
      END SELECT  !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      GOTO 9
C     
 191  WRITE (IOERR,*)'TFAR:1, Error opening file. IOSTAT=',IOST
      WRITE (IOERR,*)' LogUnit & NRECL=',IFUN,NRECL,' Num T=',II
      WRITE (IOERR,*)' file >',FILEIN(1:ILEN),'<'
      GOTO 9
C     
 7    CALL MVD (FFELP,FFTS,NFELP) ! transfer R*8 help to argument

 9    CONTINUE 
      IF (KODIN .LT. 10) THEN   ! always update unit status
        LOPN3=LOPF
      ELSE
        LFATM=LOPF
        if (FFOLE .LT. 0.) FFOLE=FFOLE-3. ! becomes -4:-6
      ENDIF    
C     WRITE(IOPM,*)'exit TFAR  FFATE=',FFATE
D     IF (IDB6.GE.4) WRITE(IOSP,*)'TFARx:FFOLE+ ',FFOLE,IFUN,LOPF
D    & ,FFTS(11)
      RETURN
C---------------- error section
      END                       ! SUBROUTINE

C==============================================================================
C==============================================================================

      SUBROUTINE TFAREAD (IODF,KR1,FFRAC,KR2,NUMH4,LISS,IOP, AAA,IRET)
C_Titl  TFAREAD  Read/interpolate records of open R*4 or R*8 direct-access file
      IMPLICIT NONE
C_Vars
      INTEGER*4 IODF            ! in. logical unit number of file 
      INTEGER*4 KR1             ! in. First record to read
      REAL*8 FFRAC              ! in. fraction of 2nd record;  <=0 Ignores  KR2
      INTEGER*4 KR2             ! in. Second record to read
      INTEGER*4 NUMH4           ! in. Size of record in words
      LOGICAL*4 LISS            ! in. file expected to be single precision
      INTEGER*4 IOP             ! in. logical unit for error message
      REAL*8 AAA(NUMH4)         ! out. Array to match file record
      INTEGER*4 IRET            ! out. Return code, 0 = OK
                                !      +1=IOSTAT error on  KR1
                                !      +2=IOSTAT error on KR2,  
                                !     +10 READ error
C_End 789012345678901234567890123456789012345678901234567890123456789012_4567890
      INTEGER*4 I,II,IOST
      REAL*4 FFR4(NUMH4)        ! array to match file record
      REAL*8 BBB(NUMH4)         ! buffer to match file record
      REAL*8 OMF                ! 1-fraction 

C IOSTAT: 0=no error,   Else, values not reliable
C         -=hit end of file    +=error during read, depends upon computer
      IRET=0 ! initiate as no errors
      II=KR1                    ! ensure against modifying argument
      IF (LISS) THEN            ! read  R*4, convert to  R*8
        READ(IODF,REC=II,ERR=81,IOSTAT=IOST)FFR4 ! read  R*4 
        CALL MV4D (FFR4,AAA,NUMH4) ! convert single to double precision
      ELSE
        READ(IODF,REC=II,ERR=81,IOSTAT=IOST)AAA
      ENDIF
      IF (IOST .NE. 0) IRET=IRET+1

      IF ( FFRAC .GT. 0.) THEN ! read 2nd record and interpolate
        II=KR2
        IF (LISS) THEN          ! read  R*4, convert to  R*8
          READ(IODF,REC=II,ERR=81,IOSTAT=IOST)FFR4 ! read  R*4 
          CALL MV4D (FFR4,BBB,NUMH4)
        ELSE
          READ(IODF,REC=II,ERR=81,IOSTAT=IOST)BBB ! read  R*8 record
        ENDIF
        IF (IOST .NE. 0) IRET=IRET+2
        OMF=1.D0-FFRAC
        DO I=1,NUMH4
          AAA(I)=OMF*AAA(I)+FFRAC*BBB(I)
        ENDDO
      ENDIF
      RETURN

 81   WRITE(IOP,*)'TFAREAD: I/O ERROR',IODF,KR1,FFRAC,KR2,NUMH4
      WRITE(IOP,*)'  " in TFAR8: LISS',LISS,II,IOST,IRET
      IRET=IRET+10
      RETURN
      END
