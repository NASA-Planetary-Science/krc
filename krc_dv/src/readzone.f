      SUBROUTINE READZONE (NZ, JPRT, ZDZ,ZDEN,ZCOND,ZSPH )
C_Titl  READZONE  KRC routine to read a depth zone table file
C_Vars  
      INCLUDE 'krcc8m.f' ! /KRCCOM/ has IMPLICIT NONE. Use MAXN1
      INCLUDE 'filc8m.f' ! /FILCOM/ need  FZONE
      INCLUDE 'unic8m.f' ! /UNITS/  need  IOD1 IOERR IERR
C_Args
      INTEGER*4 NZ   !both.  in= Size of other arguments, out= number defined
C                     If I/O error occurs, return will be negative eror code
      INTEGER*4 JPRT  !in.  Logical unit for record of table <6 = none
      
      REAL*8 ZDZ(*),ZDEN(*),ZCOND(*),ZSPH(*) !out. values for each row of table
C                 Some may be coded pointers to materials
C_Desc  Expects a file with up to 10 leading comment lines ended by  C_END line
C Followed by line of four columns,  a non-positive first column ends the table.
C_Calls  none
C_Hist 2016feb15  Hugh_Kieffer To support KRC depth zone table

      INTEGER I,J,K  ! utility variables
      INTEGER JERR  ! internal error code
      REAL*8 COL1,COL2,COL3,COL4 ! one row of layer table as read

      CHARACTER*8 BUFF
      CHARACTER*24 EMES(4) /'opening input file'
     &  ,'unexpected end-of file','reading a data line'
     &  ,'Looking for C_END'/
      JERR=0                    ! internal error code. It is in 
      print *,'IBD1:6',IDB1,IDB2,IDB3,IDB4,IDB5,IDB6

      I=LEN_TRIM(FZONE)         ! index of last non-blank character
      WRITE(*,*)'FZONE,NZ,JPRT=',FZONE(1:I),NZ,JPRT
      OPEN (UNIT=IOD1,FILE=FZONE,STATUS='OLD',IOSTAT=IERR,ERR=81)
C     Skip past the C_END line
      DO I=1,20
        READ (IOD1,'(A8)',ERR=84,END=82) BUFF ! read into character buffer
        J=INDEX(BUFF,'C_END')
        IF (IDB4.GE.3) WRITE (*,'(2i4,1x,a)') I,j,BUFF
        IF (J.GT.0 .AND. J.LT.5) GOTO 30
      ENDDO
      WRITE(IOERR,*)'Last line looking for C_END: ',BUFF
      GOTO 84

 30   J=NZ                      ! room available 
      IF (J.GT.MAXN1) J=MAXN1   ! KRC commons limit
      IF (JPRT.GT.5) WRITE (JPRT,*) ' Lines as read by READZONE'
      DO I=1,J                  ! each row of file
        READ (IOD1,*,ERR=83,END=40) COL1,COL2,COL3,COL4 ! thick dens,cond,Cp
        IF (JPRT.GT.5) WRITE (JPRT,33) I,COL1,COL2,COL3,COL4
        IF (COL1.LE.0. ) GOTO 40 
 33     FORMAT (I3,F9.4,F9.2,F9.4,F9.2)
        ZDZ(I)  =COL1
        ZDEN(I) =COL2
        ZCOND(I)=COL3
        ZSPH(I) =COL4
        K=I                     ! hold the number of valid lines
      ENDDO

 40   NZ=K                      ! number of valid rows

 9    CLOSE (IOD1)
      RETURN

C     error section --------------------------------------------
 84   JERR=JERR+1
 83   JERR=JERR+1
 82   JERR=JERR+1
 81   JERR=JERR+1
C      WRITE(IOSP,*) 'READZONE: file=',fzone,' FAILED.  IOSTAT=',IERR
C      WRITE(IOSP,*) 'ERROR ',EMES(JERR),JERR+80
      WRITE(IOERR,*) 'READZONE: file=',fzone,' IOSTAT=',IERR
      WRITE(IOERR,*) 'ERROR ',EMES(JERR),JERR+80
      NZ=-JERR                  ! return an error code
      GOTO 9
      END
