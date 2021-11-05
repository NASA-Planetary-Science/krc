      SUBROUTINE WRAPER8 (DR,D1,DELD,NUMD,DPER,FTOL, J1,FX,J2,IRET)
C_Titl  WRAPER8  Find interpolation for request time within uniformly sampled periodic function
      IMPLICIT NONE
C_Args  In.
      REAL*8 DR                 ! request time, unrestricted
      REAL*8 D1                 ! first time in file
      REAL*8 DELD               ! delta time between samples
      INTEGER*4 NUMD ! number of times available, less, same or more than a period
      REAL*8 DPER               ! length of a period (in time units)
      REAL*8 FTOL   ! fractional tolerance, should be appropriate for precision
C out
      INTEGER*4 J1              ! lower sample index
      REAL*8 FX                 ! fraction of higher sample
      INTEGER*4 J2   ! higher sample index; not reliable if fraction is le zero
      INTEGER*4 IRET            ! error flag; bit encoded  0= all  OK
             ! +1: period is not integral number of samples within TOLY below
             ! +2: time is in unsampled part of period, use an end.
C_Desc.
C  Find the indices for an interpolation interval in a periodic function 
C available at uniformly spaced "samples" within the full period
C when the samples might cover less, same or more than a full period.
C  Tries to "wrap" requested time into the sampled time range.
C  If the desired sample is not covered, set a warning flag, and use the 
C closest available sample.
C  Test against a fraction of spacing between samples; if distance to nearest 
C sample is smaller than input tolerance, use that sample only.
C  Possible cases:  A; samples cover exactly full period if wrapped
C  B: fewer than this, so some portion of period not sampled
C  C: more than this, so some redundancy. Use it for large times.
C_Liens
C Does not check that deld and dper are positive and deld << dper.
C
C_Hist 2018oct14  Hugh  Kieffer for finding seasons in  KRC far-field files
      REAL*8 TOLY /1.D-5/       !  FIRM-CODE: Tolerance on spacings per period
      INTEGER*4 I,NPY
      REAL*8 D2                 ! last time available
      REAL*8 DUSE,X,Y

D      WRITE (*,'(A,3f7.2,i5,f7.2,g12.5)'),'Inputs:'
D     &   ,DR,D1,DELD,NUMD,DPER,FTOL
      J2=0                      ! for debug only??
      IRET=0                    !  Default is all  OK
      X=DPER/DELD               ! samples per period, expect integer
      NPY=NINT(X)               ! number of samples per period
      Y=ABS(X-NPY)
      IF (Y .GT. TOLY) then 
        IRET=IRET+1             ! warning
D        print *,',DperW',DPER,DELD,X,NPY,Y,toly
      endif
      D2=D1+(NUMD-1)*DELD       ! last time available

C  Move time by integral periods to be minimally inside available time range
      IF (DR .LT. D1) THEN      ! before start of sampled time
        X=(D1-DR)/DPER          ! periods offset to first time available
        I=INT(X) +1             ! next integer
        DUSE=DR+ I*DPER         ! move by integral periods
        IF (DUSE .GT. D2) THEN  ! moved after end of sampled time
          IRET=IRET+2
          J1=NUMD
          FX=0.
D          Print *,'DUSE>D2'
          RETURN
        ENDIF               
      ELSE IF (DR .GT. D2) THEN  ! after end of sampled time
        X=(DR-D2)/DPER          ! periods offset from last time available
        I=INT(X) +1             ! next integer
        DUSE=DR-I*DPER          ! move by integral periods
        IF (DUSE .LT. D1) THEN ! moved before start of sampled time
          IRET=IRET+2
          J1=1
          FX=0.
D          Print *,'DUSE<D1'
          RETURN
        ENDIF
      ELSE
        DUSE=DR                 ! already with available time range
      ENDIF
D      write (*,'(a,3f8.3,i5)')'D1,DUSE,D2,IR',D1,DUSE,D2,IRET

C     Convert time to nearest sample index J1 and offset fraction
      
      X=(DUSE-D1)/DELD          ! desired time as positive real sample 
      J1=NINT(X)                ! nearest sample index
      Y=ABS(X-J1)               ! fractional distance from integer sample
      J1=J1+1                   ! 1-based indexing
      IF (Y .LE. FTOL) THEN     ! use close single sample
        FX=0.                   ! will not use interpolation
        IF (J1 .GT. NUMD) THEN  ! sample not available
          IRET=IRET+2
          J1=NUMD               ! use last available
D          Print *,'J1>NUMD'
          RETURN
        ENDIF
      ELSE                      ! interpolate between two samples
        J1=INT(X)               ! floor of sample index
        FX=X-J1                 ! fractional distance to next sample
        J1=J1+1                 ! 1-based indexing
        J2=J1+1                 ! sample after time
D        Print *,'J2=J1+1'
C     May need to wrap  J2
        IF (J2 .GT. NUMD) THEN 
          IF (NUMD .EQ. NPY-1) THEN  !
            J2=0                ! wrap
          ELSE IF (NUMD .GE. NPY) THEN
            J2=J2-NPY           ! move one period earlier
          ELSE 
            FX=0.               ! use first available
            IRET=IRET+2
          ENDIF
D          Print *,'J2>NUMD'
        ENDIF
      ENDIF
      
      RETURN
      END
