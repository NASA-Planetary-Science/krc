      SUBROUTINE TFAR8 (KODE,KREC,DELP, FTS,FTP,FTA)
C     _Titl  TFAR8  Open and read KRC type -N direct-access file
C     _Vars
      INCLUDE 'krcc8m.f'        ! has IMPLICIT NONE
C Here refer to:  DAYTIM  FD  ID  KFARAC  LD  MAXNH  MAXN4  NUMFD  NUMID  NUMLD 
C     INCLUDE 'latc8m.f'
C     INCLUDE 'dayc8m.f'
C     INCLUDE 'hatc8m.f'
      INCLUDE 'unic8m.f'  ! contains MINT
      INCLUDE 'filc8m.f'
C     _Args
      INTEGER*4 KODE            ! in. control current KRCCOM
C               1 = Open file .  KREC: +1=check KRCCOM  +2=stat file
C                 will set  LOPN3=.true. if open successful
C               2 = Return help about the file
C                  KREC can be used to set continuing reporting action. NOPE  
C               3 = Read a season.  input  KREC as virtual record number
C               4 = Close the file.  KREC ignored
      INTEGER*4 KREC            !in  Record to read, 1-based.
C               record 1 contains KRCCOM, record 2 is the first season
C               When  KODE=1: 0/1 = do not / do check values against KRCCOM 
      REAL*8 DELP(*)   ! out.  10+MAXN4) Info. needed to find the proper record
C   1)=number of hours defined    2)=number of latitudes defined 
C   3)=Number of seasons in file  If returned 0, (kode=2), file not found
C         If returned negative  (kode=3), MINT, then number of T-sets, invalid
C   4)=Number of Temp. Sets   5)=spare 
C   6)=first date in the fff      7) =delta day in fff      8)= emissivity
C   9)= Float(records)           10)=spare                11+) latitude values 
C Next 3 used only for KODE=3
      REAL*8 FTS(MAXNH,MAXN4) ! out. Tsurf (hour,latitude]
      REAL*8 FTP(MAXNH,MAXN4) ! out. Tplan (hour,latitude] 
      REAL*8 FTA(MAXNH,MAXN4) ! out. Tatm  (hour,latitude]
C_Lims
C_Desc
C  Can read KRC type -1,- 2, -3 files. See helplist.txt
C  Initially assumes type -1, then checks K4OUT in file and can retry
C_Hist 2016may11 Hugh Kieffer Adopt from tdisk8.f
C 2016may21 HK Enable reading files with 1,2, or 3 kinds of temperature
C_End6789012345678901234567890123456789012345678901234567890123456789012_4567890

C Make arrays that match  REAL*8 COMKRC(NWKRC)
      REAL*8 FELP(10+MAXN4)      ! file info, remembered here
C Make arrays that match COMKRC(NWKRC)   Definitions MUST MATCH KRCCOM exactly
      REAL*8 F3FD(NUMFD)        ! Real parameters
      REAL*8 F3LA(MAXN4)        ! latitude in degrees. | N4 defined
      REAL*8 F3EV(MAXN4)        ! elevation in km.     | N4=F3ID(4)
      INTEGER*4 F3ID(NUMID)     ! Integer parameters
      LOGICAL*4 F3LD(NUMLD)     ! Logical parameters
      CHARACTER*84 F3KIT        ! purpose line from the input file
      CHARACTER*20 F3DAY        ! date/time that file was created
C      COMMON /F3KCOM/ F3FD,F3LA,F3EV,F3ID,F3LD,F3KIT,F3DAY  ! to match KRCCOM
C To handle 1 or 2 or 3  T-sets
C Note: Direct write zero-fills unused part of record

      INTEGER*4 JREC    ! the virtual record number 
      INTEGER*4 PREC    ! the physical record
      INTEGER*4 NLF     ! number of latitudes defined
      INTEGER*4 MREC    ! number of seasons in the file
      REAL*4 FREC       ! " " , as a real number
      INTEGER*4 KACT     ! reporting action for each read  REPLACED by KFARAC
      INTEGER*4 I,II,ILEN,IOST,ITOP,NWTOT,NRECL
      INTEGER*4 BUFF(13) ! for file status

      SAVE KACT,JREC,MREC,FELP        ! insure these remembered
C     
      ILEN = LNBLNK(FFAR)
      IF (IDB3.EQ.2) WRITE(IOSP,*)'TFAR:0 ',KODE,KREC

      SELECT CASE(KODE)   !vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv 
   
      CASE(1)                   ! open file            1  1  1  1  1  1  1  1
        IF (LOPN3) CLOSE (IOD3)
        LOPN3=.FALSE.
C     recl may be bytes or longwords, depending upon  OS and compiler options.
        NWTOT=MAXNH*MAXN4       ! each of TSF TPF and TATM
        NRECL=8*NWTOT    ! bytes: or  NRECL=NWTOT  ! depends upon compiler <<<<
        OPEN (UNIT=IOD3,FILE=FFAR,ACCESS='DIRECT',STATUS='OLD'
     &       ,RECL=NRECL,ERR=191,IOSTAT=IOST)
        WRITE(IOSP,110)FFAR(1:ILEN),NWTOT, NRECL,DAYTIM
 110    FORMAT (/'TFAR:  Opened direct access file = ',A
     &       /' NWTOT & NRECL used=',2I7,'  Now=',A)
        IF (IDB3.GE.3) WRITE(IOSP,*)'TFARc KREC=',KREC,LOPN3,IOD3
        II=1 ! record containing KRCCOM
        READ (IOD3,REC=II,IOSTAT=IOST) F3FD,F3LA,F3EV,F3ID,F3LD
     &       ,F3KIT,F3DAY  ! Order  MUST MATCH KRCCOM exactly
        MINT= -F3ID(17)    ! negative of K4OUT in the file = Number of T sets
        LOPN3=.TRUE.            !
        NLF=F3ID(4)             ! number of latitudes defined in file
        CALL FSTAT(IOD3, BUFF)  ! get file status
        FREC= DBLE(BUFF(8))/DBLE(NRECL) ! number of records it contains
        MREC=NINT(FREC)          ! nearest integer
        FELP(1)=DBLE(F3ID(6))   ! Number of hours defined
        FELP(2)=DBLE(NLF)       ! number of latitudes defined
        FELP(3)=DBLE((MREC-1)/MINT)    ! Number of seasons in file
        FELP(4)=DBLE(MINT)      ! Number of temperature sets in file
        FELP(6)=F3FD(87)        ! DJul when first record was written
        FELP(7)=F3FD(42)        ! DELJUL between seasons
        FELP(8)=F3FD(2)         ! surface emissivity
        FELP(9)=FREC            ! check on NRECL; should be an integer
        DO I=1,NLF
          FELP(10+I)=F3LA(I)    ! transfer latitudes
        ENDDO
C        print *,'TFARaa',FFAR  !dbb
C        print *,' Ncase+', ncase,j5,j4,nrecl  !dbb
C        print *,' NLF,MREC ',NLF,MREC !dbb
C        PRINT *,' BUFF=',BUFF !dbb
C        PRINT *,' FELP',FELP !dbb
        IF (MOD(KREC,2).EQ.1) THEN !  compare parameters
          WRITE(IOSP,*)'KRCCOM Comparison:'
          DO I=1,NUMFD
           IF (F3FD(I).NE.FD(I)) WRITE(IOSP,*)'FD diff:',I,FD(I),F3FD(I)
          ENDDO   
          WRITE(IOSP,*)'Latitudes and elevations'
          WRITE(IOSP,'(10F8.2)')(F3LA(I),I=1,NLF) 
          WRITE(IOSP,'(10F8.2)')(F3EV(I),I=1,NLF)
          DO I=1,NUMID
           IF (F3ID(I).NE.ID(I)) WRITE(IOSP,*)'ID diff:',I,ID(I),F3ID(I)
          ENDDO
          DO I=1,NUMLD
          IF (F3LD(I).NEQV.LD(I))WRITE(IOSP,*)'LD diff:',I,LD(I),F3LD(I)
          ENDDO
        ENDIF

        IF (KREC.GE.2) THEN     ! print file status
          WRITE (IOPM,FMT="('TFAR: File size:',    T30,I19)") buff(8)
          WRITE (IOPM,FMT="(' Last modify. time ',A19)")CTIME(buff(10))
          WRITE (IOPM,FMT="(' Preferred block size:',T30,I19)") buff(12)
          WRITE (IOPM,FMT="(' No. blocks allocated:',T30,I19)") buff(13)
          WRITE (IOPM,*)'Records in file=',frec,mrec
        ENDIF
        GOTO 7

      CASE(2)          ! only provide help info        2  2  2  2  2  2  2  2
        KACT=KREC                ! remember the setting NO LONGER USED
        IF (LOPN3) GOTO 7       ! just return help
        DELP(3)=0.              ! Flag as no data available
        WRITE (IOERR,*) 'TFAR:2, file is not open'
C     
      CASE(3)         ! read requested season             3  3  3  3  3  3  3  3
        IF (LOPN3) THEN
          IF (MINT.LT.1 .OR. MINT.GT.3) THEN  ! somewhere Mint has been changed 
            WRITE (IOERR,*)'TFAR:3 Invalid Mint=',MINT
            WRITE (IOSP,*)'TFAR:3 Invalid Mint=',MINT
            DELP(3)=-1. ! Fatal, can transfer out-of-bounds 
          ELSE
           PREC=2+MINT*(KREC-2) ! krccom+ n*(seasons to disk already)+this
           II=PREC ! first physical record of season
           READ(IOD3,REC=II,ERR=81,IOSTAT=IOST)FTS
           II=PREC+1
           IF (MINT.EQ.2) READ(IOD3,REC=II,ERR=81,IOSTAT=IOST)FTP
           II=PREC+2
           IF (MINT.EQ.3) READ(IOD3,REC=II,ERR=81,IOSTAT=IOST)FTA
          ENDIF

          IF (KFARAC.EQ.1) WRITE(IOSP,310)FFAR(1:ILEN),KREC
 310      FORMAT(' TFAR: READ FILE= ',A,' RECORD=',I4)
          IERR=IOST
          JREC=KREC
        ELSE
          WRITE (IOERR,*)'TFAR:3, READ, BUT NO FILE OPEN'
        ENDIF
        GOTO 7
C     
      CASE(4)                   !  close the file     4  4  4  4  4  4  4  4
        IF (LOPN3) then
          CLOSE (IOD3)
          WRITE (IOSP,*)'Closed direct access file. Type=',ITOP
          LOPN3=.FALSE.
        ELSE                    ! was not open
          WRITE (IOERR,*)'TFAR:4, no file was open'
        ENDIF

      CASE DEFAULT                 ! else   else   else  else   else   else  
        WRITE (IOERR,*)'TFAR:5, Called with invalid KODE=',kode
      END SELECT  !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      GOTO 9
C     
 191  WRITE (IOERR,*)'TFAR:1, ERROR OPENING FILE. IOSTAT=',IOST
      WRITE (IOERR,*)' IOD3 & recl=',IOD3,NRECL,'  MINT & II=',MINT,II
      WRITE (IOERR,*)' file=  ',FFAR(1:ILEN)
      LOPN3=.FALSE.
      GOTO 9
C     
 7    DO I=1,10+MAXN4           ! transfer help to argument
        DELP(I)=FELP(I)
      ENDDO

 9    CONTINUE
      
C     WRITE(IOPM,*)'exit TFAR  KREC=',KREC
      IF (IDB3.GE.7) WRITE(IOSP,*)'TFARx:  KODE,KREC=',KODE,KREC
      RETURN
C---------------- error section

 81   WRITE(IOERR,*)'TFAR: read error, KREC,MINT,IOSTAT=',KREC,II,IOST
      GOTO 9

      END
