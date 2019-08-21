      PROGRAM PORBMN
C_Titl PROBMN  main program of planetary orbit (porb) software system. R*8
C  This system uses conical approximation to orbits.
C Routines interface through /porbcm/ and a file of same name
C  containing several examples.
C Most of the terminology and variable names are based on  Hugh  Kieffer's
C    geometry conventions. See matrix.tex
C
Calls:  EPHEMR   PORB1  PORBIO  PORBQQ
C lower utilities:  ANG360  CALDATE  MPRINT  MPROD3  ROTAX  ROTDIA  ROTVEC 
C_Hist circa 1968 Hugh Kieffer Original version
C 1975--mid HK l sub s corrected 
C 1978jul-- HK vax version 84may24  85jan13
C 1985----- HK convert to vector routines with input>output argument order
C 2009dec24 HK modify some names used in common
C 2014jun09 HK Go to R*8 version, older r*4 routines renamed ---4.f
C 2016mar22 HK untabify
C_End
      INCLUDE 'porbc8m.f' ! has  IMPLICIT  NONE

      INTEGER KODE
      CHARACTER CHAR*1
      CHARACTER TITLE*25, BUFFER*22
      REAL*8 DER,DEV

      PVERS ='PORB:2014jun10' ! version date  14 bytes

C Next 5 lines load items into Common
      IOK = 5 ! keyboard
      IOS = 6 ! monitor
      IOP = 3 ! print file
      IOD = 4 ! read file
      PICON = 3.14159265358979D0
      R2D=180.D0/PICON

1     WRITE(IOS,*) '?* OUTPUT TO PRINTER OR TERMINAL: [P]/T?'
      READ(IOK,*,END=9,ERR=1) CHAR
      IF(CHAR.EQ.'T' .OR. CHAR.EQ.'t') THEN
        IOP=IOS
      ELSE
C       UNIX has no equivalent to VMS ASSIGN statement
C       CALL ASSIGN(IOP,'PORB.PRT')
        OPEN(UNIT=IOP,FILE='porb.prt')
      ENDIF
 2      WRITE(IOS,3)
 3      FORMAT(' ? Option Kode   0=quit'
     &/' 1 = Initiate COMMON from an orbital elements file'
     &/' 2 = Read/Save COMMON from/to ASCII or binary file '
     &/' 3 = print Ephemeris tables based upon current COMMON'
     &/' 4 = print COMMON'
     &/' 5 = test computation and print of rotation matrices')
      READ(IOK,*, ERR=2,END=9)KODE
      IF(KODE.LT.1) GOTO 9      ! quit
      IF(KODE.GT.5) GOTO 2      ! invalid, ask again
      CALL CATIME (BUFFER)      ! TITLE is temporary buffer
      RUNTIM=BUFFER(2:21)
      WRITE(IOP,7) PVERS,RUNTIM
7     FORMAT(' PORBMN:',A,'  Run_time=',A)
      GOTO (10,20,30,40,50),KODE

 10   CALL PORBIG (TITLE)      ! Read orbital elements from disk files. Initiate porbcm
      GOTO 2

20    CALL PORBIO (TITLE) ! Read/write porb common to disk file with name = porbcm.dat
      GOTO 2

30    CALL EPHEMR
      GOTO 2

40    WRITE (IOP,*)'Object = ',TITLE ! print common
      CALL PRTPCOM (3) ! Print the PORB system version 3 common
      GOTO 2

C50      CALL PORBQQ
C      GOTO 2

 50   CALL ROTSHO('BFRM',BFRM,IOP) ! print the rotation matrix
      CALL ROTEST(BFRM, DER,DEV)   ! check that it is one
      WRITE (IOP,*) 'Delta Determ., max Dev =',DER,DEV
C      IF (DEV.GT.TOL1  .OR.  DER.GT.TOL2)  print *,'not a rotation matrix'
      GOTO 2
C
9     STOP
      END
