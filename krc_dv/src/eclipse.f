       SUBROUTINE ECLIPSE (PARC,PARI, JBE,OUTT)
C_Titl  ECLIPSE  Calculate insolation profile through an eclipse
C_Vars
      IMPLICIT NONE
      INTEGER*4 NPARC, NPARI
      PARAMETER (NPARC=12)
      PARAMETER (NPARI=2)
      REAL*8 PARC(NPARC)        ! in. parameters
C  NOTATION ctime means coarse time-steps, as used in  TDAY
C           ftime means fine time-steps, as used in  TFINE
C           "time-steps" may be either, depending upon context.
C parc in. fldarr(10)   comments below use 1-based indices to agree with fortran
C    1:  Style: 0=none  1=Daily  1.3+=Rare, and  NINT is the layer factor
C    2:  Distance to sun,  AU (used to get  Sun angular diameter)
C    3:  Occulting body  (OB) radius, km
C    4:  Mutual orbit radius, km
C    5:  Eclipsed body radius km (significant for phobos/mars)
C    6:  Mutual solar synodic period, days
C    7:  Eclipse path closest approach to  OB center, as fraction of  OB radius
C    8:  J2000 date of eclipse (not used here)
C    9:  Eclipse central hour ( of 24)
C   10:  Debug code. ne.0 prints constants and >1 prints that point in detail.
C           negative values will cause return with all  OUTT as 1.
C   11:  Current latitude on  EB, degrees
C   12:  Solar period of the  EB
C    x:  Extinction scale height of  OB's atmosphere, km  NOT implimented
      INTEGER*4 PARI(NPARI)  ! in. parameters
C    0:  Number of coarse time steps per period
C    1:  Logical unit to use for error messages
      INTEGER*4 JBE(*) ! both.  in: (1)=0 means do only the indices  =1: and  OUTT
C       out: Indices of the first and last  ctime-steps in eclipse,
C                 in the system specified by  PERIOD and  N2 
C         1:2] = earliest and latest possible eclipse ctimes
C         3:4] = ctime range of calculations for the current latitude
      REAL*8 OUTT(*)       ! both, insolation multiplier, the fraction of the
!   un-eclipsed insolation.  If daily,  N2 steps will be defined, covering the 
!   entire sol.  If rare, time steps are smaller by  K=  round(PAR(1)) squared,
!  K*(JBE(4)-JBE(3)+1)  will be defined, and apply to ctime-steps begining
!   at  JBE(3)-1.  If error occurs, first value will be negative.
C_Desc  Derived to echo  eclipse.f.  See that for explanation
C   coded and tested in  IDL with  circleoverlap.pro
C  Decide to keep inputs in physical units and normalize internally.
C  All obscuration calculations are done in units of time steps.
C  Formula from  http://mathworld.wolfram.com/Circle-CircleIntersection.html
C   coded and tested in  IDL as circleoverlap.pro
C
C  For daily, computes insolation factors for the full day on the  KRC time grid.
C  For rare: uses finer time steps for only the  KRC ctime steps covering
C   the eclipse, but ensures they correspond to complete  ctime-steps
C   i.e.,  (JBE(4)-JBE(3)) * layer_factor^2  starting at start of  JBE(3)
C
C  The instant of time-step 1 for insolation is 0-based +1/2 timestep into
C   the sol, or 1-based -1/2 timestep.
C
C_Hist 2017mar11:Apr03  Hugh  Kieffer for  KRC eclipses
C 2017dec06  HK  Commments and case only
C  Comments use 2 spaces before uppercase to support the  FLOWER algorithm
C 2018jan22  HK  Major revision.  Deal with local bias and non-sync periods
C 2018jan28 Convert to FORTRAN
C 2018feb15 Fix  rare timing; DANG if not rare; some name changes.
C_End 
C        parc=[ 0.,  5.2026,71492.,0.6711D6,3.551,0.01,6000.,12.,20,1]
C        pari=[1536,6]
      REAL*8 ALAP               ! area of overlap between two circles
      REAL*8 AU /149.5978707D6/ !  Astronomical unit, km
      REAL*8 SEQR /695700./     !  Sun equitorial radius, km
      REAL*8 DAYSEC /86400.D0/  ! seconds/day
      REAL*8 DHALF /0.5D0/      ! 1/2
      REAL*8 PIVAL /3.1415926536D0/ ! pi
      REAL*8 FRAB
      REAL*8 FRAK ! fraction of insolation that gets to target
      REAL*8 SRAD
      REAL*8 OBR
      REAL*8 RMO
      REAL*8 EBR
      REAL*8 VEL
      REAL*8 BIKM ! sun-line miss-distance from center of ob,
      REAL*8 CN2
      REAL*8 SUNR
      REAL*8 PHI
      REAL*8 HALFT
      REAL*8 B,R  ! radii of the bigger and smaller intersecting circles,scaled
      REAL*8 B2,R2   ! square of  B and  R after scaling
      REAL*8 BPR,BMR !  B+R and  B-R after scaling
      REAL*8 ASUN,PSI,COSZ, DANG,DTIF,DTIM,DN2
      REAL*8 HALFX,HN2, MUPER,PANG,QS,SINLAT,SOLD,STEF
      REAL*8 THETA,V1,V1X,V2,YH,YHX,YY,Z2
      REAL*8 D,D2,TWOD ! circle center separatiion after scaling, squared and *2
      REAL*8 P1,P2,P3,P4,PP  ! parts of sqrt term in overlap, and their product
      REAL*8 ANG1,ANG2, SQP  ! parts of the overlap equation
      REAL*8 EHOUR,MUSEC
      REAL*8 SEPR ! distance from  OB to surface of  EB, km
      REAL*8 ZENA ! zenith angle of the eclipse, radians
      REAL*8 FMIN, TFAC
      REAL*8 FE7,FE8 ! ctime-steps at the beginning and end of eclipse

      REAL*8 DTOR,TWOPI
      INTEGER*4 J,J7,J8,N2,NOUT
      INTEGER IOERR,KODE,KFAC ! local: could never exceed  I*2 range
      LOGICAL LRARE ! do only eclipse, and use fine-time

C      IOFF=0 ! index offset from fortran to current base system

      DTOR=PIVAL/180.D0      ! degrees to radians factor
      TWOPI=2.0D0*PIVAL      ! 2 pi

      N2=PARI(1)               ! number of coarse time steps in a period
      DN2=DBLE(N2)
      IOERR=PARI(2)            ! error unit
      LRARE = (PARC(1) .GE. 1.3)   ! eclipse on single day
      KFAC=NINT(PARC(1))      ! layer factor
      SRAD=SEQR/(PARC(2)*AU)   ! solar radius, radians
      OBR=PARC(3)              !  Occulting body  (OB) radius, km
      RMO=PARC(4)              !  Mutual orbit radius, km
      EBR=PARC(5)              !  EB radius
      MUPER=PARC(6)            !  EB orbit period, days
      EHOUR=PARC(9)             ! surface hour at the center of eclipse
      KODE=NINT(PARC(10))     ! debug kode
      THETA=DTOR*PARC(11)      ! latitude on the  EB in radians
      SINLAT=SIN(THETA)          ! sin of latitude
      FRAB=PARC(7)+SINLAT*EBR/OBR           ! b'= fractional bias
      PANG=DHALF*PIVAL-THETA-ATAN(FRAB * OBR/RMO)! p= co-latitude of sub-ob
      SOLD=PARC(12)                         ! sol of  EB in days
      PHI=PIVAL*(EHOUR/12. -1.)   ! phi, longitude from noon
      BIKM=FRAB*OBR              ! sun line closest approach to  OB center, km
      MUSEC=MUPER*DAYSEC         !  EB orbit period, seconds
      KFAC=KFAC*KFAC             ! time factor
      TFAC=DBLE(KFAC)          ! time factor as real
      VEL=TWOPI*RMO/MUSEC        ! orbital velocity, km/sec
      DTIM=MUSEC/DN2            ! size of one ctime step, seconds
      COSZ=SIN(PANG)*COS(PHI)    ! cosine of zenith angle
      ZENA=ACOS(COSZ)            ! zenith angle of eclipse on eq., radian

      SEPR=RMO-COSZ*EBR   ! distance between  OB and  surface of  EB, km
      SUNR=SEPR*SRAD      ! apparent radius of sun at  OB from  EB surface, km

      CN2= DN2*EHOUR/24.0D0 ! real timestep at center of eclipse
      IF (BIKM .GE. (SUNR+OBR)) THEN  ! no eclipse
        JBE(3)=NINT(CN2)        ! ctime closest to eclipse
        JBE(4)=JBE(3)-1         ! later is lower. impossible: signals no eclipse
        RETURN
      ENDIF
      DANG=TWOPI/DN2      ! delta anomoly in a coarse-time step
      IF (OBR .GE. SUNR) THEN   ! ensure that  B is the bigger
        B=OBR                   ! radius of bigger object
        R=SUNR                  ! of smaller
      ELSE
        B=SUNR
        R=OBR
      ENDIF
! Compute longest possible eclipse =   when b'=0
      YHX=OBR+SUNR              ! greatest eclipse extent, km
      YH=SQRT(YHX**2-BIKM**2) ! eclipse half length in km
      PSI=ASIN(YH/SEPR)        ! half-angle of eclipse, radian
      IF (EBR.LT.OBR) THEN  !  EB is smaller, assume lunar eclipse
        HALFT=PSI*MUPER/TWOPI  ! half-time in days
        HALFX=ASIN(YHX/SEPR)*MUPER/TWOPI ! maximum possible
      ELSE                      ! solar eclipse
        V1X=TWOPI*EBR/SOLD      ! maximum L-plane surface vel., km/day
        V1=V1X*COSZ             !  L-plane surface vel., km/day
        V2=TWOPI*RMO/MUPER      !  L-plane shadow velocity, km/day
        HALFT=YH/(V2-V1)        ! half-time in days
        HALFX=YHX/(V2-V1X)      ! maximum " "
      ENDIF
      HN2=HALFX*DN2/SOLD         ! max.  half-time in coarse time-steps
      JBE(1)=INT(CN2-HN2)       ! round down
      JBE(2)=INT(CN2+HN2)+1     ! round up
      HN2=HALFT*DN2/SOLD         ! half-time in coarse time-steps
      FE7=CN2-HN2               ! ctime in steps at start of eclipse
      FE8=CN2+HN2               !  " " at  end of "
      J7=INT(FE7)               ! index of ctime step containing eclipse start
      J8=INT(FE8)+1             ! " " " " end
      JBE(3)=J7                 ! transfer indices to output
      JBE(4)=J8

      DTIF=DTIM/TFAC            ! fine time step, seconds
      STEF=VEL*DTIF             ! satellite motion in a fine time step, km
C---- some meanings change
      B=B/STEF                  ! convert sizes to units of time steps
      R=R/STEF
      Z2=(BIKM/STEF)**2         ! square of closest approach
      QS=RMO/STEF               ! orbital radius in steps
      R2=R**2                   ! terms that appear more than once
      B2=B**2
      BPR=R+B
      BMR=B-R

      IF (OBR .GE. SUNR) THEN    !  Sun is smaller;  =R.  Total eclipse
        ASUN=PIVAL*R2          ! area of the  Sun in step^2 units
        FMIN=0.                ! fraction of sunlight if complete overlap
      ELSE          !  Sun is larger;  =B.  Annular eclipse
        ASUN=PIVAL*B2          ! area of the  Sun in step^2 units
        FMIN=1.D0-R2/B2        ! fraction of sunlight if complete overlap
      ENDIF
C?      dang=stef/rmo            ; central angle change in one time step, radian
 33   FORMAT(A,5G13.5,/,5G13.5)
      IF (KODE .GT. 0) THEN 
        WRITE(*,33)  'PARC',PARC
        WRITE(*,33)  'MUPER,N2,ZENA,SEPR',MUPER,N2,ZENA,SEPR
        WRITE(*,33)  'LRARE,KFAC,TFAC,FMIN',LRARE,KFAC,TFAC,FMIN
        WRITE(*,33)  'SRAD,OBR,RMO,MUSEC', SRAD,OBR,RMO,MUSEC
        WRITE(*,33)  'VEL,BIKM,DTIM,SUNR',VEL,BIKM,DTIM,SUNR
        WRITE(*,33)  'YH,PSI,DTIF,STEF',YH,PSI,DTIF,STEF
        WRITE(*,33)  'CN2,HALFT,hn2,FE7,FE8',CN2,HALFT,HN2,FE7,FE8
        WRITE(*,33)  'B,R,Z2,QS',B,R,Z2,QS
        WRITE(*,33)  'B2,R2,BPR,BMR',B2,R2,BPR,BMR
        WRITE(*,33)  'ASUN,DANG',ASUN,DANG
        WRITE(*,*) 'JBE',(JBE(J),J=1,4)
      ENDIF

      IF (LRARE) THEN     ! using fine time steps thru eclipse
        NOUT=(J8+1-J7)*KFAC     ! always corresponds to integral time-steps
        CN2=(CN2-J7)*TFAC       ! ftime at eclipse center, relative to  J7 time
        J7=1                    ! limits for fine-time loop
        J8=NOUT                 ! " " 
        DANG=DANG/TFAC          ! delta anomaly in a fine-time step
      ELSE                ! using c-time steps all day
        NOUT=N2
      ENDIF
      CN2=CN2+DHALF             ! adjust insolation time to center of interval
      CALL FILLD (1.D0,OUTT,NOUT) ! to handle steps outside eclipse
      IF (KODE.LT.0) RETURN ! debug test
      DO J=J7,J8     !  Daily=all,  Rare= only the points in eclipse
        PSI=ABS(DBLE(J)-CN2)*DANG   ! orbital angle from eclipse center. radian
        YY=SIN(PSI)*QS        ! distance from the eclipse centerline in steps
        D2=YY**2+Z2
        D=SQRT(D2)             ! separation of circle centers, in steps
        IF (D .LE. BMR) THEN 
          FRAK=FMIN              ! total overlap
        ELSE IF (D .GE. BPR) THEN
          FRAK=1.              ! no overlap
        ELSE
          TWOD = 2.D0*D   ! next: four terms in square root.
          P1=BMR-D              ! -d+r-r = (b-r)-d
          P2=-BMR-D             ! -d-r+r = -(b-r) -d
          P3=BPR-D              ! -d+r+r = (b+r)-d
          P4=BPR+D              !  d+r+R = (b+r)+d
          PP=P1*P2*P3*P4        ! product of 4 terms in radical

          IF (PP .LT. 0.) THEN
            WRITE(IOERR,33)  'ECLIPSE.F: NEGATIVE SQRT'
            OUTT(1)=-1.D0
            RETURN
          ENDIF

          SQP=SQRT(PP)
          ANG1= ACOS((D2+R2-B2)/(TWOD*R)) ! angles in the overlap diagram
          ANG2= ACOS((D2+B2-R2)/(TWOD*B)) ! " "
          ALAP= R2*ANG1 +B2*ANG2   -DHALF*SQP ! area of overlap
          FRAK=1.-ALAP/ASUN     ! fraction of sun not obscured
          IF  (J-J7 .EQ. KODE-1) THEN
            WRITE(*,33) 'J,PSI,YY,D,PP',J,PSI,YY,D,PP
            WRITE(*,33) 'SQP,ANG1,ANG2,FRAK',SQP,ANG1,ANG2,FRAK
          ENDIF
        ENDIF
        OUTT(J)= FRAK ! proportion of sunlight that reached  target=EB
      ENDDO

      RETURN
      END
