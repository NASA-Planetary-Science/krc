      SUBROUTINE PORBIO (TITLE)
C_Titl  PORBIO  read/write porb common to disk file with name = porbcm.dat
      INCLUDE 'porbc8m.f'      ! has  IMPLICIT  NONE
C_Args 
      CHARACTER*(*) TITLE       !both. object name
C_Calls: [open, read, write, close]
C_Hist  Hugh Kieffer 76aug02. vax version 84may28
C 2012nov22 HK change NAME to FILE in OPEN statements
C 2013feb07  HK Use  IMPLICIT  NONE
C 2014jun09 HK Untabify Use R*8 common
C_End6789012345678901234567890123456789012345678901234567890123456789012_4567890

      INTEGER MSEC /15/         ! max geometry record
      INTEGER IO                ! in/out flag
      INTEGER ISEC              ! index of geometry matrix in file
      INTEGER ITYPE,I           ! 
      CHARACTER FNAME*30
      CHARACTER*4 FROMTO(2) /'FROM',' TO '/
C binary=unformatted, direct access; PORBCM.DAT
C ASCII=formatted, ?? access ; PROBCM.INP

 1    WRITE (IOS,*)' ?* 1=READ FROM FILE  2=SAVE NEW RECORD'
      READ (IOK,*,ERR=1,END=9)IO
      IF (IO.LT.1) GOTO 1
 11   WRITE (IOS,*)' ?* 1=BINARY (UNFORMATTED)   2=ASCII (FORMATTED)'
      READ (IOK,*,ERR=11,END=9)ITYPE
      IF (ITYPE.GT.1) GOTO 30

C open binary data file
      OPEN (UNIT=IOD,FILE='PORBCM.DAT',FORM='UNFORMATTED'
     &,ACCESS='DIRECT',STATUS='UNKNOWN',RECL=IDEM1)
C note: if -xl[d] is set, RECL is number of 4-byte words, else number of bytes
 2    WRITE (IOS,*)' ?* RECORD IN FILE PORBCM.DAT?'
      READ (IOK,*,ERR=2,END=9)ISEC
      IF (ISEC.LT.1 .OR. ISEC.GT.MSEC) THEN
        WRITE (IOS,*)'MAXIMUM RECORD =',MSEC
        GOTO 2
      ENDIF
      IF (IO.EQ.1) THEN
        READ  (IOD,REC=ISEC) PCOM
        TITLE='From Binary: unknown'
      ELSE
        WRITE (IOD,REC=ISEC) PCOM
      ENDIF
      WRITE (IOP,7) FROMTO(IO), ISEC,PLANUM,TC
 7    FORMAT ('0PORBIO: /PORBCM/ ',A,' PORBCM.DAT RECORD',I2,
     &     '  IPLAN,TC=',2F6.3,/)
      GOTO 9

C open formatted ascii record for possible inclusion in an ascii file.
 30   FNAME='PORBCM.mat'
      WRITE (IOS,*)' ?* NAME OF ASCII OUTPUT FILE; DEFAULT IS ',FNAME
      READ  (IOK,*,ERR=30,END=9) FNAME
      IF (IO.EQ.1) THEN         ! read
        OPEN (UNIT=IOD, FILE=FNAME, STATUS='OLD',ACCESS='SEQUENTIAL')
        READ (IOD,31) RUNTIM,PLANUM,TC,TITLE !  first line
 31     FORMAT(15x,A20,11X,F6.1,F8.5,1X,A9)
        READ (IOD,33) PCOM
 33     FORMAT (5G15.7)         ! format for the geometry matrix
      ELSE                      ! write
        OPEN (UNIT=IOD, FILE=FNAME, STATUS='UNKNOWN',ACCESS='APPEND')
        I=LEN_TRIM(TITLE)       ! planet
        print *,'PORBIO@30 >',title,'<',i
        WRITE (IOD,34) PVERS,RUNTIM,PLANUM,TC,TITLE(1:I)
C34     FORMAT (1X,A20,'=RUNTIME.  IPLAN AND TC= ',F5.1,F8.5,X,A)
 34     FORMAT (A14,1x,A20,' IPLAN,TC= ',F5.1,F8.5,1X,A)
        WRITE(IOD,33) PCOM
      ENDIF
      WRITE (IOP,*)' ASCII RECORD ',FROMTO(IO),' FILE =',FNAME
 9    CLOSE (IOD)
      RETURN
      END
