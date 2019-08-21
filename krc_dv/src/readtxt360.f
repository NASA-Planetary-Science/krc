      INTEGER FUNCTION READTXT360 (FNAME,IOD, XXX,YYY)
C_Titl  READTXT360  Read 2-column text file of REAL values
      IMPLICIT  NONE
C_Args
      CHARACTER*80 FNAME        ! in. File name
      INTEGER IOD               ! in. Available logi. unit
      REAL XXX(*)               ! both.  Values from first column, [date]
      REAL YYY(*)               ! both.  Values from 2nd  column
      INTEGER KK                ! Function out Number of defined entries

C_Desc
C Reads an Text table of two columns, white-space separated
C Columnar table must be immediately preceeded by a line beginning:  C_END
C Before that may be up to 9 lines of free-form comments
C Assumes first column is Modulo 360, and prepends and appends values
C  to avoid later wrap-around
C Returns -1 if error occurs
C_Hist  Hugh_Kieffer  2006sep09
C 2008mar31 HK integer >> INTEGER
C_End6789012345678901234567890123456789012345678901234567890123456789012_4567890

C Local variables
      INTEGER MROW,MSKIP
      PARAMETER (MROW=362)      ! max number of table entries
      PARAMETER (MSKIP=10)      ! max number of lines to skip through C_END
      INTEGER IOERR             ! logi. unit for reporting errors
      PARAMETER(IOERR=6)
      CHARACTER*80 RBUF
      REAL XIN,YIN
      INTEGER IOST              ! error status
      INTEGER I,J

      KK=-1                   ! possible error flag
      
D      WRITE(*,*)'FNAME=',fname
      OPEN (UNIT=IOD,FILE=FNAME,STATUS='OLD',IOSTAT=IOST,ERR=81)
C     Skip past the C_END line
      DO I=1, MSKIP
        READ (IOD,'(A80)',ERR=82,END=30) RBUF ! read into character buffer
D        WRITE (*,*) I,RBUF
        J=INDEX(RBUF,'C_END')
        IF (J.GT.0 .AND. J.LT.5) GOTO 30
      ENDDO

C read single lines until a negative season [ indicating all done]
 30   DO I=2, MROW-1
        READ (IOD,*,ERR=83,END=40), XIN,YIN 
D       WRITE (*,*) I,XIN,YIN
        XXX(I)=XIN
        YYY(I)=YIN
        KK=I
      ENDDO

C Fabricate first and last point to avoid wrap-araound later
 40   CONTINUE
      XXX(1)=XXX(KK)-360.       ! wrap last point to negative LS
      YYY(1)=YYY(KK)
      KK=KK+1
      XXX(KK)=XXX(2)+360.       ! wrap first point to > one year
      YYY(KK)=YYY(2)

 9    READTXT360=KK
D      write (*,*)' READT kk=',kk
      CLOSE (IOD)
      RETURN

C error section
 81   WRITE(IOERR,*)'READTXT360 error opening input file =',FNAME
      WRITE(IOERR,*),'IOSTAT=',IOST
      GOTO 9

 82   WRITE(IOERR,*)'READTXT360 unexpected end-of file ',FNAME,I
      GOTO 9
 
 83   WRITE(IOERR,*)'READTXT360 error reading a data line',FNAME,I
      GOTO 9

      END
