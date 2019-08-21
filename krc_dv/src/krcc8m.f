C_Titl  krcc8m.f = KRCCOM common for input and transfer variables
C_Limitations 
      IMPLICIT NONE             ! none-the-less, try to code with usage
      INTEGER*4 MAXN1,MAXN2,MAXN3,MAXN4,MAXN5,MAXN6,MAXNH,MAXBOT
     &,MAXN1P,NUMFD,NUMID,NUMLD,N4KRC,NWKRC,KOMMON,MAXN4E,MAXFF
C Here are all the dimension-defining parameters for items in any common
      PARAMETER (MAXN1 =1000)     ! dimension of layers
      PARAMETER (MAXN2 =384*4*256)  ! dimension of times of day
      PARAMETER (MAXN3 =16)     ! dimension of iteration days
      PARAMETER (MAXN4 =37)     ! dimension of latitudes
      PARAMETER (MAXN4E =38)    ! " "  Even needed for LATCOM NDJ4
      PARAMETER (MAXFF=384*4*4) ! dimension of far-field times of day
      PARAMETER (MAXN5 =2161)    ! dimension of saved seasons
      PARAMETER (MAXN6 =6)      ! dimension of saved years
      PARAMETER (MAXNH =86400)     ! dimension of saved times of day, multiple of 24
      PARAMETER (MAXBOT=14)      ! dimension of time doublings  MUST  BE  EVEN
      PARAMETER (MAXN1P=MAXN1+1) ! dimension layer temperature points
      PARAMETER (NUMFD=96, NUMID=40, NUMLD=20) ! number of each type
      PARAMETER (N4KRC=NUMFD*2+NUMID+NUMLD+2*MAXN4*2+104/4) ! # of 4-byte words
              ! above is size of common in 32-bit words. it  MUST  BE  EVEN
      PARAMETER (NWKRC=N4KRC/2) ! number of 8-byte words in krccom. Used by tdisk
      PARAMETER (KOMMON=512000000) ! Storage used by tdisk

      INTEGER*4 N1,N2,N3,N4,N5,  N24,IIB,IC2,NRSET,NMHA              !  1:10
     &,NRUN,JDISK,IDOWN,I14,I15,  KPREF,K4OUT,JBARE,NMOD,IDISK2   ! 11:20
     &,KOLD,KVALB,KVTAU,ID24(2), KFARAC,NBKRC,NFD,NID,NLD                ! 21:30
     &,N1M1,NLW,JJO,KKK,N1PIB,  NCASE,J2,J3,J4,     J5            ! 31:40

      REAL*8 ALB,EMIS,SKRC,COND2,DENS2, PERIOD,SPHT,DENS,CABR,AMW          ! 1:10
     2,ABRPHA,PTOTAL,FANON,TATM,TDEEP, SPHT2,TAUD,DUSTA,TAURAT,TWILI ! 11:20
     3,ARC2,ARC3,SLOPE,SLOAZI,TFROST, CFROST,AFROST,FEMIS,AF1,AF2    !   :30
     4,FROEXT,FD32,RLAY,FLAY,CONVF, DEPTH,DRSET,PHOG,GGT,DTMAX        !   :40
     5,DJUL,DELJUL,SDEC,DAU,SUBS, SOLCON,GRAV,ATMCP ! :48
     &,HUGE,TINY,EXPMIN,FSPARE,FLOST,RGAS,TATMIN,PRES,OPACITY,TAUIR  ! :74
     &,TAUEFF,TATMJ,SKYFAC,TFNOW,AFNOW,PZREF,SUMF,TEQUIL,TBLOW,HOURO ! :84
     &,SCALEH,BETA,DJU5,DAM,EFROST,DLAT,COND,DIFFU,SCALE ! :93
     &,PIVAL,SIGSB,RADC      ! :96

      LOGICAL*4 LP1,LP2,LP3,LP4,LP5,  LP6,LPGLOB,LVFA,LVFT,LKOFT     !  1:10
     &,LPORB,LKEY,LSC,LZONE,LOCAL,   LD16,LD17,LD18,LD19,LONE        ! 11:20

      REAL*8 CCKU(4),CCKL(4),CCPU(4),CCPL(4) ! coef of K & Cp, Upper/Lower layers
C      INTEGER*1  KITLE(84),DAYTIM(20) ! Sum= 104 bytes MUST be multiple of 8
      CHARACTER*20 DAYTIM       ! date/time that job stars
      CHARACTER*84 KITLE        ! purpose line from the input file
      REAL*8 ALAT(MAXN4)        ! latitude in degrees. set in  TCARD
      REAL*8 ELEV(MAXN4)        ! elevation in km. set in  TCARD
      REAL*8    FD(NUMFD)
      INTEGER*4 ID(NUMID)
      LOGICAL*4 LD(NUMLD)

C_Description.  
C  Cset -> routine which sets value.  seas=TSEAS  lat=TLAT
C *= initially set as input, this routine may reset value.
C lines 1-5 = real: input via  TCARD
C         6 =   " : transfer
C       7-8 = integer: input via  TCARD
C       9-A =    "   : transfer
C       B-C = logical: input via  TCARD
C       D   = title (input via tcard) and run_time (transfer)

      COMMON /KRCCOM/
     1 ALB,EMIS,SKRC,COND2,DENS2, PERIOD,SPHT,DENS,CABR,AMW          !10 1:10
     2,ABRPHA,PTOTAL,FANON,TATM,TDEEP, SPHT2,TAUD,DUSTA,TAURAT,TWILI !10 :20
     3,ARC2,ARC3,SLOPE,SLOAZI,TFROST, CFROST,AFROST,FEMIS,AF1,AF2    !10 :30
     4,FROEXT,FD32,RLAY,FLAY,CONVF, DEPTH,DRSET,PHOG,GGT,DTMAX        !10 :40
Cset                                      *day1
     5,DJUL,DELJUL,SDEC,DAU,SUBS, SOLCON,GRAV,ATMCP    !8   :48
Cset     4   4     4    4                            v
     5,CCKU,CCKL,CCPU,CCPL,   HUGE,TINY          ! 4*4+2=18  :66
     &,EXPMIN,FSPARE,FLOST,RGAS,TATMIN,PRES,OPACITY,TAUIR,TAUEFF,TATMJ  !10 :76
Cset   *seas       ----seas-------
     6,SKYFAC,TFNOW,AFNOW,PZREF,SUMF, TEQUIL,TBLOW, HOURO,SCALEH,BETA !10 :86
Cset   -----------lats--------- tint ---lats-------      ---lats----
     6,DJU5,DAM,EFROST,DLAT,COND,  DIFFU,SCALE,PIVAL,SIGSB,RADC       !10 :96
     &,ALAT,ELEV                                       ! 2*maxn4=2*37=74 :170
Cset   seas lat day2   lats -------day1------- --main------
     7,N1,N2,N3,N4,N5,  N24,IIB,IC2,NRSET,NMHA                       !  1:10
     8,NRUN,JDISK,IDOWN,I14,I15,  KPREF,K4OUT,JBARE,NMOD,IDISK2      ! 11:20
     9,KOLD,KVALB,KVTAU,ID24, KFARAC,NBKRC,NFD,NID,NLD               ! 21:30
Cset             ----card---
     A,N1M1,NLW,JJO,KKK,N1PIB,  NCASE,J2,J3,J4,     J5               ! 31:40
Cset   ---day1- lat ----day1-    main -day2- lats  seas
     B,LP1,LP2,LP3,LP4,LP5,    LP6,LPGLOB,LVFA,LVFT,LKOFT            !  1:10
     C,LPORB,LKEY,LSC,LZONE,LOCAL,   LD16,LD17,LD18,LD19,LONE        ! 11:20
     D, KITLE,DAYTIM  ! 
Cset   tcard tprint tcard tcard 
C
      EQUIVALENCE (FD(1),ALB), (ID(1),N1), (LD(1),LP1) ! alignment

C_Hist  85may12  Hugh Kieffer  USGS_Flagstaff major revision
C  86jul01  HK add comments of where variable is set
C  97feb11  HK revise many locations and sizes
C  97sep08  HK add  SCALEH,BETA   97sep11 replace  DIMENSION
C  98MAY26  HK remove  L from implicit integer
C  00jan23  HK redefine  BK->COND2  BRC->DENS2  FD16->SPHT2
C 2002mar01 HK Make implict L logical*4
C 2002mar07 HK LD20-->LONE.  move ALAT & ELEV from LATCOM, make id(21)=kold
C 2002jul16 HK Add OPACITY to common
C 2004jul07 HK Move dimension-defining parameters from other commons into KRCCOM
C 2008oct02-15 HK Move MAXBOT from DAYCOM to here. Replace ID22 with KVALB & 
C    KVTAU and redefine ABRAMP to be AMW. New maximum dimensions.
C 2008nov13 HK Add T-dependent conductivity parameters.
C 2010jan12 HK Change to IMPLICIT NONE  Change  PI>>PIVAL  RAD>>RADC
C 2010feb17 HK Include CCPx, extending common. Rename 8 items to CCKU,CCKL
C 2012may10 HK Define FLOST
C 2014feb25 HK Specify all word lengths as *4
C 2014mar10 HK Make  REAL*8  version    Change name from krccom.inc Untabify
C 2014may04 HK Increase N2 by factor of 256
C 2016mar:may HK Move ALAT,ELEV from near end. Change IB to IIB,  IC to IC2
C             LNOTIF to LZONE
C 2016aug12 HK Add NBKRC to common
C 2016sep09 HK Add KFARAC to common  Set by TCARD 
C 2017apr06 HK Move MAXFF here from  HATCOM
C 2018feb01 Replace  DDT with PHOG
C_End _______________________________________________________________________
 
