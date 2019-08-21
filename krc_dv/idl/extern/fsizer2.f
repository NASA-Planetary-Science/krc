      SUBROUTINE FSIZER2(NRECL,KSIZE,NRFILE,NAMFIL)
C_Titl  FSIZER  Opens/reads ephemeris file to determine word and file sizes
      IMPLICIT NONE
      SAVE
C_Args
      Integer*4 NRECL ! output. Number of bytes in a IO record element.
C                     ! will be set negative if bytes must be swapped
      Integer*4 KSIZE ! output  Number of single precision words in a record.
C                       will be 0 if file open failed
      Integer*4 NRFILE  ! output. Internal unit number for the ephemeris file
      CHARACTER*80 NAMFIL       ! output. Name of ephemeris file
C_Vars
      INCLUDE 'chrcom.inc'      ! /CHRHDR/ CNAM,TTL
C_Desc
C  This subroutine opens the file, 'NAMFIL', with a phony record length, reads 
c  the first record, and uses the info to compute KSIZE, the number of single 
c  precision words in a record.  
C
C_Hist  2006jan21  Code pulled from ssd.jpl.nasa.gov//pub/eph/export/DE405/
C   Original code by Myles Standish of JPL.    Extract routine from testeph.f
C 2006jan21 Hugh_H_Kieffer  Use IMPLICIT NONE, rearrange argument documentation
C             Added some printout
C 2009mar22 HK Minor cleanup, check that calcs are same as new version from JPL
C           Convert all test code into D lines
C 200jun28 HK deactivate swap-bytes capability, change input file name
C_End __________________________________________________________________________

      DOUBLE PRECISION SS(3),AU,EMRAT
      INTEGER I,J,KHI, KMX,MRECL,NCON,ND, NUMDE
      INTEGER IPT(3,13)

C  *****************************************************************
C
C  THE PARAMETERS NRECL, NRFILE, AND NAMFIL ARE TO BE SET BY THE USER
C
C  *****************************************************************

C  NRECL=1 IF "RECL" IN THE OPEN STATEMENT IS THE RECORD LENGTH IN S.P. WORDS
C  NRECL=4 IF "RECL" IN THE OPEN STATEMENT IS THE RECORD LENGTH IN BYTES
C  (for UNIX, it is probably 4)
C
      NRECL= 4                  ! HHK 2006jan21

      NRFILE=12 ! the internal unit number used for the ephemeris file

C  NAMFIL IS THE EXTERNAL NAME OF THE BINARY EPHEMERIS FILE

      NAMFIL='JPLEPH'           ! HHK 2009jun28


C  *****************************************************************
C  *****************************************************************

C  **  Open the direct-access file and get the pointers in order to 
C  **  determine the size of the ephemeris record

      MRECL=NRECL*1000
D      WRITE(*,*)'FSIZER2a: nrecl,mrecl,namfil',nrecl,mrecl,namfil ! hhk

      OPEN(NRFILE,
     *       FILE=NAMFIL,
     *       ACCESS='DIRECT',
     *       FORM='UNFORMATTED',
     *       RECL=MRECL,ERR=88,  ! HK add error label
     *       STATUS='OLD')
D      WRITE(*,*)'FSIZER2b: after OPEN' ! hhk

C  Word types:           ch   ch f8  int f8    f8 ! hhk
      READ(NRFILE,REC=1)TTL,CNAM,SS,NCON,AU,EMRAT,
     * ((IPT(I,J),I=1,3),J=1,12),NUMDE,(IPT(I,13),I=1,3) ! all Integer hhk
D      write(*,*)'FSIZER2: after READ' ! hhk
D      write(*,*)'NCON,AU,EMRAT,numde=',NCON,AU,EMRAT,numde ! hhk
      CLOSE(NRFILE)

C-      IF (iabs(NCON).GT.20000) THEN   ! must swap bytes hhk
C-        NRECL=-NRECL            ! flag this need to calling routine hhk
C-        CALL SWAPBYTE4(IPT,39,IPT) ! hhk
C-      ENDIF


C  FIND THE NUMBER OF EPHEMERIS COEFFICIENTS FROM THE POINTERS

      KMX = 0
      KHI = 0

      DO I = 1,13
         IF (IPT(1,I) .GT. KMX) THEN
            KMX = IPT(1,I)
            KHI = I
         ENDIF
      ENDDO

      ND = 3
      IF (KHI .EQ. 12) ND=2
D      write(*,*)'KHI=',khi  ! hhk
D      write(*,*)'IPT(*,khi)=',(ipt(i,khi),i=1,3)  ! hhk
      KSIZE = 2*(IPT(1,KHI)+ND*IPT(2,KHI)*IPT(3,KHI)-1)
      write(*,*)'FSIZER2: before return KSIZE=',ksize ! hhk

      RETURN

 88   WRITE(*,*)'FSIZER2 Failed to open file=',NAMFIL
      KSIZE=0             ! set as error flag
      RETURN

      END
