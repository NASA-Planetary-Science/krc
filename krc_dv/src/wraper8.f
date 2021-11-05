      SUBROUTINE WRAPER8 (DR,D1,DELD,NUMD,DPER,FTOL, J1,FX,J2,IRET)
C_Titl  WRAPER8  Find interpolation for request time within uniformly sampled periodic function
      IMPLICIT NONE
      INCLUDE 'unic8m.f'      ! need only for IDB4,
C_Args  In.
      REAL*8 DR                 ! request time, unrestricted
      REAL*8 D1                 ! first time in file
      REAL*8 DELD               ! delta time between samples
      INTEGER*4 NUMD            ! number of times available
      REAL*8 DPER               ! length of a period (in time units)
      REAL*8 FTOL   ! fractional tolerance, should be appropriate for precision
C out
      INTEGER*4 J1              ! lower sample index
      REAL*8 FX                 ! fraction of higher sample
      INTEGER*4 J2   ! higher sample index; not reliable if fraction is le zero
      INTEGER*4 IRET            ! error flag; bit encoded  0= all  OK
             ! +1: period is not integral number of samples within FTOL
             ! +2: time is in unsampled part of period, use an end.
             ! +4:  in unsampled part of period, and FX negative
C_Desc.
C  Find the indices for an interpolation interval in a periodic function 
C available at uniformly spaced "samples" within the full period
C when the samples might cover less, same or more than a full period.
C  Tries to "wrap" requested time into the sampled time range.
C  If the desired sample is not covered, set a warning flag, and assume
C first sample would wrap to end of year
C  Possible cases:  A; samples cover exactly full period if wrapped
C  B: fewer than this, so some portion of period not sampled
C  C: more than this, so some redundancy. Use it for large times.
C_Liens
C Does not check that deld and dper are positive and deld << dper.
C
C_Hist 2018oct14  Hugh  Kieffer for finding seasons in  KRC far-field files
C 2019mar23 HK Revise and simplify logic.
      INTEGER J,K  ! TEMPORYR
      INTEGER*4 I,NPY
      REAL*8 D2,DN                 ! period end, last time available
      REAL*8 DUSE,X,Y

C Step 1, check period
D     WRITE (*,'(A,3f7.2,i5,f7.2,g12.5)'),'WRAPER8 Inputs:'
D    &   ,DR,D1,DELD,NUMD,DPER,FTOL
      IRET=0                    !  Default is all  OK
      X=DPER/DELD               ! samples per period, expect integral
      NPY=NINT(X)               ! number of samples per period
      Y=ABS(X-NPY)
      IF (Y .GT. FTOL) IRET=IRET+1             ! warning       
D        print *,',DperW',DPER,DELD,X,NPY,Y,ftol

C Step 2  Offset by integral periods to be inside available time range
      DUSE=DR
      D2=D1+DPER       ! one year after first time
      DN=D1+(NUMD-1)*DELD ! last sample 
      IF (DR .LT. D1) THEN      ! before start of sampled time
        X=(D1-DR)/DPER          ! periods offset to first time available
        I=INT(X) +1             ! next integer
        DUSE=DR+ I*DPER         ! move by integral periods
      ELSE IF (DR .GT. MAX(D2,DN)) THEN  ! after end of primary year time
        X=(DR-D1)/DPER          ! periods offset from first time available
        I=INT(X)                ! floor integer
        IF (I.GT.0) DUSE=DR-I*DPER          ! move by integral periods
      ENDIF  
   
C Step 3  Convert time to lower index J1 and fraction; Interpolate 
C   It is possible for duse to be larger than dper \
C  Assume that NUMD +1 points would provide overlap         
      X=(DUSE-D1)/DELD          ! desired time as positive real sample
      J1=INT(X)                ! 0-based lower sample index
      IF (J1 .LT. NUMD-1) THEN    ! simple interpolation
        FX=X-J1                 ! fractional distance to next sample
        J1=J1+1                 ! 1-based indexing
        J2=J1+1                 ! sample after time
      ELSE                      ! Beyond uniform samples, treat as wrap
        IF (J1 .GT. NUMD) IRET=IRET+2  ! warning, in undefined region
        FX=(DUSE-DN)/(D2-DN) ! fraction of the interval without samples
        IF (FX .LT. 0) IRET=IRET+4 ! impossible ?
        J1=NUMD
        J2=1
      ENDIF

      j=ISHFT(IDB4,-2)
      k=MOD(j,2) 
C     IF (MOD(ISHFT(IDB4,-2),2) .EQ. 1)   ! 4's bit true
D     IF (k                     .EQ. 1)   ! 4's bit true
D    & WRITE (*,'(A,3F8.3,I5)')'D1,DUSE,D2,IR', D1,DUSE,D2,IRET

      RETURN
      END
