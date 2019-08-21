      SUBROUTINE MSEAS (DATE,IOD1,IOD2,FILENAME, IER)
C_Titl  MSEAS: Interpolate thermal models to the desired season
      IMPLICIT NONE
C_vars 
      INCLUDE 'modcom.inc'      ! dimensions, and to hold results
C Will fill  THLM(NHOUR,NLAT,NMOD2) with thermal models at the desired date
      INCLUDE 'errcom.inc'      ! debug print units and counts
C_arg
      REAL*4 DATE               !in. julian date for which model is desired
                                ! base 2,440,000
C				!  should be <~0.5 days from observations time
      INTEGER IOD1              !in. logical unit (lun) to use for  FILENAME
      INTEGER IOD2              !in. lun to use for reading model files
      CHARACTER*(*) FILENAME    !in. path to file of list of model file names
      INTEGER IER               !out. error code; 0=normal +=error number

C_Files
C  FILENAME must point to a file containing a list of filenames.
C  In that file, the first line must contain an integer indicating the style 
C  of the first record:   1=kieffer  else=Mellon
C  followed by the directory name, ending with a /, in single quotes. 
C  The list of files must correspond to the
C    order of model parameters stated in  modcom.inc
C  The model files should be  Temperature-only records 
C     except for the first record being:
C for  Kieffer:  COMKRC &  ALAT &  ELEV & filler
C for  Mellon,  ASCII values of model parameters
C_Desc
C Can read files from either kieffer or mellon, these differ only in the format
C   of the first record, which is ascii for mellon. 
C  For each model: 
C -  Opens the file, check the critical parameter values;
C -  Determines the records which seasonally bound the requested date;
C    reads that pair of adjacent records, and interpolates linearly to the 
C    requested date.
C -  Do splint fit in hour, and replicate midnight to 0 hour; placing the results
C    in common  THLM (temperatures) and  DHLM (spline derivatives)
C -  Closes the model file.
C_Lims
      INTEGER NUMERR
      PARAMETER (NUMERR=12) ! number of error messages in this routine
C array sizes  MUST agree with sizes in common.
C  KRC runs must have had  K4OUT=-1 so that records of  TSF were written
C_Calls  BINHEAD2  LNBLNK  R2R  SPLINE  SWAPBYTE4  
C_History
C   97jan29  Hugh_H_Kieffer  USGS_Flagstaff original version
C   97sep10  HHK  add check of input model parameter values
C   97oct31  HHK add handling of  Mellon files
C   98apr24  HHK version 3; convert to bi-cubic splines for hour/latitude
C   98may05  HHK increase file name from *60, reverse defaults for  VT
C     &      change from normalized pressure to  P in  Pascals
C   98may28  HHK version 4; for large model set
C   98jun10  HHK change  MAXN4 from 19. minor comment changes
C   98jul13  HHK 2 fixes for  Mellon models not integral with  Mars year
C 2008jan29  HHK Add comments and printout. 
C        &    Check if byte swap required. Use swapbyte4 instead of swapbyter4
C        &    Fix errors in RN5(not used) and DJR2 for Kieffer models
C        &    Get global mean pressure at 0 elev from KRCCOM, Was fixed 689.7
C        &    Use NLAT instead of MAXN4, putting #lats definition into the common
C 2008mar28 HK Add EXTERNAL and REAL*4 for AVERAG   include krccom.inc
C_End6789012345678901234567890123456789012345678901234567890123456789012_4567890

C File header definition
      INTEGER*4  NUMFD,NUMID,NUMLD ! MUST be compatible with krccom.inc
      PARAMETER (NUMFD=96) !<< number of each type was 80 before 2008nov
      PARAMETER (NUMID=40, NUMLD=20) ! number of each type
      REAL*4    FD(NUMFD)       !  KRCCOM real array
      INTEGER*4 ID(NUMID)       !  KRCCOM integer array
      LOGICAL   LD(NUMLD)       !  KRCCOM flags array
      INTEGER*4 BD(25)          !  KRCCOM 100 bytes   
      REAL*4 ALAT(NLAT)         ! latitude in degrees
      REAL*4 ELEV(NLAT)         ! elevation in km

      REAL*4 TSF1(NHOUR,NLAT),PSF1(NHOUR,NLAT) ! hold first season array
      REAL*4 TSF (NHOUR,NLAT),PSF (NHOUR,NLAT) ! hold latest record

      CHARACTER*80 FDIR         ! directory containing model files
      CHARACTER*80 FDISK        ! name of a thermal model file
      CHARACTER*80 FNAME        ! full path name of a thermal model file
      CHARACTER*40 ERRMES(NUMERR) ! error messages, defined in error section
      CHARACTER C_ARR(7104)     ! buffer for  Mellon header

      REAL*4 YEAR / 686.98 /    ! length of  Mars year in julian days
      REAL*4 HOURP3(NHP3),TBUF(NHP3),DBUF(NHP3),TDAY(NHOUR,2)

C Local variables
      LOGICAL DOSWAP            ! if need to swap bytes
      INTEGER JW,KW             ! indices for debug write
      INTEGER I,J,K,KBAD,KT,KIEMEL,IOS,LENREC
      INTEGER NGRID,NHREC,N4,N5,N24,NWTOT
      INTEGER JF,JP,JT,JA,JI,JR,JMOD,IBAD,IOP
      INTEGER K4OUT,J5,JDISK,NREC,KREC,KREC2,KRECL ! related to disk records
      REAL*4 ALB,SKRC,TAUD,SCALEH,DJUL,DELJUL,PRES,PTOTAL,DATE1,RN5
      REAL*4 FRAC,F1,DJR2,RECF
      REAL*4 AMOD,DATEOFF       ! intrisic function & its results
      EXTERNAL LNBLNK,AVERAG
      INTEGER LNBLNK  ,LDSK,LDIR ! intrisic function & its results
      REAL*4 AVERAG
      REAL*4 YP /2.E30/ ! flag for spline to use 'natural' ends

      IOP=IDB1                  ! debug output if not 0
C      IOP=IOERR                 !<<< all write to the same unit  !
      KW=7                      ! set <1  for no write
      JW=20                     ! set <1  for no write

      IER=0
      K=0                       ! point before any model file
C Get expected array sizes from  COMMON included from  KRC system
      NGRID=NHOUR*NLAT
      NWTOT=2*NGRID        !  4-byte words in a record
C      write(ioerr,*)'NWTOT=',nwtot
      LENREC=NWTOT*4 ! LINUX uses bytes in a record 
C      write(ioerr,*)'NWTOT,LENREC=',nwtot,lenrec

      DO I=1,NHP3               ! make hour array for spline fit
         HOURP3(I)=HOUR1+(I-3)*HOURDEL ! -1,0,1, ...24,25
         ENDDO
      CALL R2R (HOURP3(2),HOURS,NHP1) ! make 0:24 hour array for spline interp.
      IF (IOP.NE.0) then 
         WRITE(IOP,*)'HOURP3=',HOURP3 
         WRITE(IOP,*)'HOURS=',HOURS
      endif

C Open file of model file names
      OPEN (IOD1,FILE=FILENAME,ERR=881,IOSTAT=IOS,STATUS='OLD')
      READ (IOD1,*,ERR=891) KIEMEL,FDIR ! get who & directory part of path
      LDIR = LNBLNK(FDIR)

      NHREC=1                 ! first record is header, rest are  T's only
      IF (KIEMEL.EQ.1) THEN       ! must reset some parameter values
         VP(2)=600.             !<<<<< dummy code
      ENDIF

C Read list of model file names and
C determine floating-point record that corresponds to desired  JD
        KBAD=0                  ! count  out-of-order files
        KRECL=-1
        JMOD=NIA*NTAU           ! # models in one elevation set


      DO K=1,NMOD               ! for each model file =======================
        J=MOD(K,JMOD)           ! Each major model group
        READ (IOD1,*,ERR=882,END=883) FDISK ! construct the filename
        LDSK=LNBLNK(FDISK)
        FNAME=FDIR(1:LDIR)//FDISK(1:LDSK)
C Open it and read  header from first record
        OPEN (UNIT=IOD2,FILE=FNAME,ACCESS='DIRECT',STATUS='OLD'
     &       ,RECL=LENREC,ERR=884,IOSTAT=IOS)
        I=1                     ! # header records
        IF (KIEMEL.EQ.1) THEN   ! -------------- Kieffer model ---------  
           READ (IOD2,REC=I,ERR=885,IOSTAT=IOS)FD,ID,LD,BD,ALAT,ELEV
C  Check if byte swap is required
           PTOTAL=FD(12) ! Global annual mean surface pressure at 0 elev., Pascal
           DOSWAP=(PTOTAL.LT.200. .OR. PTOTAL .GT. 2000.) ! test for unreasonable
           IF (DOSWAP) THEN     ! byte-swap numerical arrays in header
              CALL SWAPBYTE4(FD,NUMFD,FD)
              CALL SWAPBYTE4(ID,NUMID,ID) ! defined as INTEGER*4
              CALL SWAPBYTE4(LD,NUMLD,LD) ! defined as LOGICAL*4
              CALL SWAPBYTE4(ALAT,NLAT,ALAT)
              CALL SWAPBYTE4(ELEV,NLAT,ELEV)
           ENDIF
C Extract items from file header record
           ALB=FD(1)            ! Surface albedo
           SKRC=FD(3)           ! Surface thermal inertia
           PTOTAL=FD(12) ! Global annual mean surface pressure at 0 elev., Pascal
           TAUD=FD(17)          ! Mean visible opacity of dust
C           SCALEH=FD(69)        ! initial atmospheric scale height
           SCALEH=FD(14)*FD(70)/(FD(10)*FD(47))        !  Tatm*Rgas/AMW*grav)
           DJUL=FD(41)          ! Starting Julian date of run -2440000
           DELJUL=FD(42)        ! Increment between seasons in Julian days
           N4=ID(4)             ! Number of latitudes
           N5=ID(5)             ! Number of seasons run, including spin-up
           N24=ID(6)            ! Number of 'hours' per day stored
           JDISK=ID(12)         ! Season count that disk output began
           K4OUT=ID(17)     ! Disk output control: -=KRCCOM(once), then TSF & TPF
           J5=ID(40)            ! Index of current "season"
           PRES = PTOTAL*EXP(-ELEV(1)/SCALEH) ! effective pressure
           RN5=YEAR/DELJUL      ! # seasons per martian year.
           DJR2=DJUL+(JDISK-1)*DELJUL ! Julian date of first season on file
           NREC=N5-JDISK+1      ! # seasonal records 
           IF(J.EQ.1 .AND. IOP.NE.0) THEN 
              WRITE(IOP,*)'DOSWAP,ID=',DOSWAP,ID 
              WRITE(IOP,*)'ALB,SKRC,TAUD=',ALB,SKRC,TAUD
              WRITE(IOP,*)'ELEV(1)',ELEV(1)
              WRITE(IOP,*)'into scaleh',FD(14),FD(70),FD(10),FD(47)
              WRITE(IOP,*)'All FD'
              DO KT=1,24 
                 JT=4*KT
                 JI=JT-3
                 WRITE(IOP,'(5(F10.2,I3))')(FD(I),I,I=JI,JT)
              ENDDO
              WRITE(IOP,*)'PTOTAL,SCALEH,PRES=',PTOTAL,SCALEH,PRES
              WRITE(IOP,*)'DJUL,DELJUL,RN5,NREC=',DJUL,DELJUL,RN5,NREC
           ENDIF
        ELSE                    ! -------------------- Mellon model -------

           READ (IOD2,REC=I,ERR=892,IOSTAT=IOS) C_ARR
           CALL BINHEAD2 (ALB, C_ARR, DELJUL, SCALEH, SKRC !  H is invalid
     &          , DJUL, N4, N24, TAUD, PRES )
           SCALEH=SCALEHM       ! use mean scale height, smae as  KRC
           RN5=YEAR/DELJUL      ! # seasons per martian year
           NREC=84              ! # seasonal records
           DJR2=10322.34        ! =djul-2.44e6 ! date of first seasonal record
           PRES=100.*PRES       ! convert from millibar to  Pascal
           IF (K.EQ.1 .AND. IOP.NE.0)
     &          WRITE (IOP,*)'BINHEAD:',DELJUL, SCALEH, DJUL,RN5
        ENDIF                   ! ---------------- end Mellon model ---------
        
C Confirm array sizes; disagreement is fatal
        IF (N24.NE.NHOUR .or.N4.NE.NLAT ) then ! in_file ne in_common
           WRITE(IOERR,'(A,6I6)')'ID(1:6)',(ID(I),I=1,6)
           WRITE(IOERR,'(A,62I6)')'Common NHOUR,NLAT = ',nhour,nlat
           GOTO 886 ! must quit
        endif
C Check parameter values; error message if files are not in order expected
        JF=(K-1)/(NIA*NTAU)     ! # full elevation sets thus far
        JP=JF+1                 ! pressure index
        JR=K-JF*NIA*NTAU        ! residue
        
        JF=(JR-1)/NIA           ! # full opacity sets
        JT=JF+1                 ! opacity index
        JR=JR-JF*NIA            ! residue
 
        JF=(JR-1)/NUMI          ! # full albedo sets
        JA=JF+1                 ! alb index
        JR=JR-JF*NUMI           ! residue
        
        JI=JR                   !  I index

        IBAD=0                  ! bit-pattern sum of errors
        IF (ABS(SKRC/VI(JI)-1) .GT. 0.1 ) IBAD=IBAD+1
        IF (ABS(ALB -VA(JA)) .GT. 0.001 ) IBAD=IBAD+2
        IF (ABS(TAUD-VT(JT)) .GT. 0.001 ) IBAD=IBAD+4
        IF (ABS(PRES/VP(JP)-1) .GT. 0.5 ) IBAD=IBAD+8 ! <<<<<< Liberal
        IF (IBAD.NE.0) THEN
           KBAD=KBAD+1          ! increment count
          WRITE (IOERR,301)IBAD, K,FNAME,SKRC,ALB,TAUD,PRES,
     &          JI,JA,JT,JP,VI(JI),VA(JA),VT(JT),VP(JP)
 301       FORMAT('Mismatch',I3,' for model',I3,2X,A
     &,/'Iner,Alb,Tau,Pres=',4F10.3,4I3
     &,/'  Expected values=',4F10.3)
        ENDIF

C Compute  DATE1, a julian date at same season as requested date,
C and within the year of the model file
        DATE1=DATE              ! desired offset julian date 
        IF (DATE1.LT.DJR2) THEN ! ensure after starting date of models
          FRAC = (DJR2-DATE)/YEAR ! determine how many years to add
          DATE1 = DATE + INT(FRAC+1.)*YEAR
        ENDIF
        DATEOFF=AMOD(DATE1-DJR2,YEAR) ! location in the year on file
        
C Compute required records to bound date requested
        RECF=DATEOFF/DELJUL     ! floating-point record desired
        KREC=INT(RECF)+1+NHREC       ! early season record
        KREC2=KREC+1            ! late season record
        FRAC=AMOD(RECF,1.)
        IF (KREC2.GT.NREC+NHREC) THEN 
           KREC2=NHREC+1        ! season wrap around
           IF (KIEMEL.EQ.2) FRAC=FRAC/.42485 ! last  Mellon interval smaller
Cfrom Mike IF (KIEMEL.NE.1) FRAC=FRAC/.57492 ! last  Mellon interval smaller
        ENDIF
        IF (KREC.NE.KRECL) WRITE(IOERR,*)'@K New KREC',K,KREC
        KRECL=KREC              ! save for next comparison for 
C Read the early and late season records
        READ (IOD2,REC=KREC  ,ERR=888,IOSTAT=IOS) TSF1,PSF1
        READ (IOD2,REC=KREC2 ,ERR=888,IOSTAT=IOS) TSF,PSF
C For Mellon files, this is the first binary read. Decide if swap needed
        IF (K.EQ.1 .AND. KIEMEL.EQ.2) 
     & DOSWAP=(TSF(1,1).LT.120. .OR. TSF(1,1).GT.340.) 
C Check if byte swap is required
        IF (DOSWAP) THEN 
           CALL SWAPBYTE4(TSF1,NGRID,TSF1)
           CALL SWAPBYTE4(PSF1,NGRID,PSF1)
           CALL SWAPBYTE4(TSF,NGRID,TSF)
           CALL SWAPBYTE4(PSF,NGRID,PSF)
           IF (IOP.NE.0 .AND. K.EQ.1) WRITE (IOP,*)
     1          'Sample T was ',F1,' Swapped bytes'
        ENDIF
        CLOSE (IOD2)

        IF (IOP.NE.0 .AND. K.EQ.KW) THEN
          WRITE (IOP,*)'k,fdisk,DATEOFF,RECF,KREC,KREC2,I,FRAC='
          WRITE (IOP,333)K,FDISK(1:LDSK),DATEOFF,RECF,KREC,KREC2,FRAC
 333      FORMAT (I4,2X,A,2X, F10.2,F7.3,2I4,F7.4)  
          WRITE (IOP,*) 'N5,JDISK,DJR2,RN5,NREC',N5,JDISK,DJR2,RN5,NREC  
          WRITE (IOP,*) 'PTOTAL,SCALEH,PRES',PTOTAL,SCALEH,PRES
          WRITE(IOP,*)'Next is TSF1 for every 5th latitude'
          DO J=1,NLAT,5
             WRITE (IOP,334) (TSF1(I,J),I=1,N24,4)
 334         FORMAT (10F10.2)
          ENDDO
        ENDIF

C Interpolate array to desired season fraction, and save
        F1=1.-FRAC
        DO J=1,NLAT
           DO I=1,N24
              TDAY(I,1) = F1*TSF1(I,J) + FRAC*TSF(I,J) ! surface  T
              TDAY(I,2) = F1*PSF1(I,J) + FRAC*PSF(I,J) ! planetary  T
           ENDDO
C Copy a diurnal curve into a buffer extended to include wrap-around,
C so that any hour from 0 to 24 can be interpolated directly, and so that
C the spline fit has one extra point on each end to minimize end-effects.
           DO KT=1,2                ! surface and planetary
              CALL R2R(TDAY(1,KT),TBUF(3),NHOUR) ! transfer 24 hours
              TBUF(2)=TBUF(NHOUR+2) ! copy midnight to before first
              TBUF(1)=TBUF(NHP1)    ! copy one before midnight
              TBUF(NHP3)=TBUF(3)    ! copy first to after midnight
              CALL SPLINE(HOURP3,TBUF,NHP3,YP,YP,DBUF) ! get coefficents
              I=K+(KT-1)*NMOD       ! set index for output model
              CALL R2R (TBUF(2),THLM(1,J,I),NHP1) ! store  T's. 25 hours
              CALL R2R (DBUF(2),DHLM(1,J,I),NHP1) ! store  derivatives
              IF(IOP.NE.0 .AND. K.EQ.KW .AND. J.EQ.JW ) THEN
                 WRITE(IOP,*)'J,KT,I=',J,KT,I
                 WRITE(IOP,*)'TDAY=',(TDAY(JF,KT),JF=1,NHOUR)
                 WRITE(IOP,*)'TBUF=',TBUF
                 WRITE(IOP,*)'DBUF=',DBUF
                 WRITE(IOP,*)'THLM=',(THLM(JF,J,I),JF=1,NHP1)
              ENDIF
           ENDDO
        ENDDO

        DO KT=1,2               ! for Tk and Tp
           I=K+(KT-1)*NMOD      ! set index of model
           TPOLE(1,I)=AVERAG(THLM(1,1,I),NHOUR) ! south pole T
           TPOLE(2,I)=AVERAG(THLM(1,NLAT,I),NHOUR) ! north pole T
        ENDDO
        
      ENDDO                     ! end of read_model loop ================
      CLOSE (IOD1)
      IF (KBAD .GT. 0) GOTO 889
      
 99   RETURN
      
      DATA ERRMES /'opening list of files'      ! 1
     &, 'reading a file name'                   ! 2
     &, 'end-of-file reading a file name'	! 3
     &, 'opening a model file'			! 4
     &, 'reading first record in model file'	! 5
     &, 'dimension of hour/lat does not match' 	! 6
     &, 'spare' ! 7
     &, 'reading desired record in model file'	! 8
     &, 'Some files out of order'               ! 9 
     &, 'spare'     ! 10 
     &, 'reading the directory for files'       ! 11
     &, 'reading Mellon first record' /         ! 12
C        1234567890123456789012345678901234567890< 40 characters
 892    IER=IER+1
 891    IER=IER+1
 890    IER=IER+1
 889    IER=IER+1
 888    IER=IER+1
 887    IER=IER+1
 886    IER=IER+1
 885    IER=IER+1
 884    IER=IER+1
 883    IER=IER+1
 882    IER=IER+1
 881    IER=IER+1
        WRITE(IOERR,8833)K,KBAD,IER,IOS,ERRMES(IER)
 8833   FORMAT('MSEAS: K,KBAD & error_# & IOS= ',I3,I4,I4,I6,1X,A)
        WRITE (IOERR,*)'possibly relevent File= ',FNAME
        GOTO 99
        END
