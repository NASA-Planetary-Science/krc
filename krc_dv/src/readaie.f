      SUBROUTINE READAIE(IK,NK,FAME, AAA,RRR,VVV, IER)
C_Titl  READAIE  Read files of albedo,inertia and elevation, return matched lines
      IMPLICIT NONE             ! none-the-less, try to code with usage
      INTEGER IK                !in. line offset: -= read entire file
C                                      += offset to first line requested 
      INTEGER NK                !in.  Number of lines to transfer
      CHARACTER*(*) FAME(4)     !in. DIR and  3 file names, Ignored if IK >0
      REAL AAA(*)               !out. Albedo, real value
      REAL RRR(*)               !out. Thermal inertia; SI
      REAL VVV(*)               !out. Elevation in km
      INTEGER IER ! out error code  In as logical unit number when IK <=0

      INTEGER NLIN,NSAM,NPAR,MEEP ! ,HEADW
      PARAMETER(NSAM=2880,NLIN=1440,NPAR=3,MEEP=80) ! Max DIMENSIONS IN FILE
      REAL PANE(NSAM,NLIN)        ! one PANE in input map, entire file
      REAL TRIP(NSAM,MEEP,3)        ! strip storage
      CHARACTER*30 ERRMES(5) /'File open failure','hit end of file' 
     + ,'read error','NK must be at least 1'
     + ,'request outside stored range'/
c      character*4 sss(3) /'Alb','Ti','Topo'/  ! required file order
      CHARACTER*40 BUF,GUF
      INTEGER I,J,k,M           ! generic and return codes
      INTEGER IOD,KEEP,IOCHECK
      INTEGER K0    ! Line offset from start of file to strip retained
      SAVE TRIP,K0,KEEP
c      LOGICAL DOB /.false./ ! .true./ !
C_Calls  BINF5  MVR  WHITE0
      INTEGER*4 WHITE0          ! Function names

      CHARACTER*40 AFILE        ! to hold full file name
C_Desc
C Discovered FORTRAN limitations that require reading the entire file in 
C a single read, and first byte or word must be 0. This constrains efficiency 
C Files expected to be  R*4 (nsamp,nline)   3=[alb,iner.elev]
C  When called with  IK lt 1, opens files, [reades header and skips -i lines],
C  Reads entire file, Transfers  nk lines starting after -IK into saved array
C  When  IK 1 or more, extracts that relative line from the saved array, 
C unscales it into  AAA,RRR,VVV
C_Use
C Must call first with IK negative and FAME defined and NK adequate
C  then can call repeatedly with increasing IK to get successive lines
C_Hist 2011aug15:19  Hugh  Kieffer  To read map input file for mkrc or krca
C 2012feb26  HK  Remove unused variables
C 2018jan22  HK  Replace  R2R with  MVR.  Clear up comments
C_End

      I=IER                     ! may be logical unit to use
      IER=0                     ! default error return is no error
      J=NSAM*NPAR               ! number of 4-byte words in a strip 
      print *,'READAIE_0: ',nsam,npar,ik,nk ! ,dob
      IF (IK.LE.0) THEN         ! ---------- open the file

         IOD=I                  ! define the logical unit number
         write(*,*) 'At 2 IOD=',iod
         K0=-IK                 ! remember the line offset
            KEEP=NK             ! number of lines to store
            write(*,*)'K0,KEEP=',K0,KEEP
            IF (KEEP.LT.1) GOTO 84 ! error, must store some

         M=WHITE0(FAME(1),GUF)  ! remove any blanks in directory
         DO 50 K=1,3               ! loop over 3 parameters
            J=WHITE0(FAME(K+1),BUF)  ! remove any blanks in file name
            AFILE=GUF(1:M)//buf(1:j)
            print *,'AFILE=',AFILE 
            i=0 ! error within this loop
            OPEN (IOD,FILE=AFILE, FORM='unformatted' 
     +           ,STATUS='OLD',IOSTAT=IOCHECK,ERR=81)
 41         print *,'OPEN iocheck=',IOCHECK
            READ(IOD,END=82,ERR=83,IOSTAT=IOCHECK) PANE ! read entire file
 42         print *,'READ iocheck=',iocheck
C FORTRAN read of IDL write skips the first byte and the last is trash
C So here, knowing the polar values are not real, simply offset 1 word
            PRINT *,PANE(1,1),PANE(NSAM-1,NLIN),PANE(NSAM,NLIN)
            PANE(NSAM,NLIN)=PANE(NSAM-1,NLIN) ! replace bad last value
            CLOSE (IOD)           ! close the file
            J=NSAM*KEEP-1         ! modest number of lines
c            if (k0+k.ge. 
            CALL MVR(PANE(1,K0+1),TRIP(2,1,K),J) ! save a strip
            TRIP(1,1,K)=TRIP(2,1,K) ! copy 2nd word of storage file
            GOTO 50
C Error section
 83         I=I+1               ! read error
 82         I=I+1               ! hit end of file
 81         I=I+1               ! File open failure
            WRITE(*,*)'IER=', I,IOCHECK,ERRMES(I),K,IK,KEEP
            IER=I               ! ensure not left at 0
            if (I.eq.1) goto 41
            goto 42

 50       CONTINUE ! end of loop

         write(*,*) 'One pixel'
         write(*,*) (TRIP(nsam/2,1,i),i=1,3) ! a pixel
      ELSE                      ! ---------- return one line

         I=IK                   ! line number in storage 
         IF (I.GT.KEEP) GOTO 85 ! outside stored range
         j=NSAM
         CALL MVR (TRIP(1,I,1),AAA,J) ! extract one line from strip
         CALL MVR (TRIP(1,I,2),RRR,J) ! extract one line
         CALL MVR (TRIP(1,I,3),VVV,J) ! extract one line
         DO I=1,j            ! convert for each pixel
            VVV(I)=VVV(I)/1000. ! convert from meter to km
         ENDDO
      ENDIF 
      GOTO 9

C ERROR SECTION          
 85   IER=IER+1                 ! request outside stored range
 84   IER=IER+4                 ! NK must be at least 1
      WRITE(*,*)'IER=', IER,IOCHECK,ERRMES(IER),I,IK,KEEP

 9    RETURN
      END
