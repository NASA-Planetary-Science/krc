C_Titl  latcom8.f   common for latitude-dependant items in  KRC
      INTEGER*4 NWLAT   ! size of common in real (4 or 8 byte)  words
      PARAMETER (NWLAT= (8+ 3*MAXN1 + 2*MAXNH)*MAXN4 +MAXN4E/2 )! Number of Real words
C_arg: all are set in  TLATS
      REAL*8 DTM4(MAXN4)         ! rms temperature change on last day
     &,TST4(MAXN4)       ! predicted equilibrium temperature of ground
     &,TTS4(MAXN4)       ! predicted mean surface temperature for each latitude
     &,TTB4(MAXN4)       ! predicted mean bottom temperature
     &,FROST4(MAXN4)     ! predicted frost amount kg/m^2.
     &,AFRO4(MAXN4)      ! frost albedo.  
     &,TTA4(MAXN4)       ! predicted final atmosphere temperature  
     &,TTX4(MAXN4)       ! spare
     &,TMN4(MAXN1,MAXN4) ! predicted convergence midnight temperature
     &,TIN(MAXN1,MAXN4)  ! minimum hourly layer temperature
     &,TAX(MAXN1,MAXN4)  ! maximum hourly layer temperature
     &,TSF(MAXNH,MAXN4)  ! final hourly surface temperature
     &,TPF(MAXNH,MAXN4)  ! final hourly planetary temperature
      INTEGER*4 NDJ4(MAXN4E) ! # days to compute solution for each latitude
C_Var: all are set in  TLATS
      COMMON /LATCOM/ DTM4,TST4,TTS4,TTB4,FROST4,AFRO4,TTA4,TTX4
     &,TMN4,TIN,TAX,TSF,TPF,NDJ4

C_Desc
C_Hist  85may12  Hugh_H_Kieffer    97feb11  HHK add  ELEV
C  97sep08  HHK add  TPF
C 2002mar09 HHK  Move ALAT & ELEV from here to KRCCOM
C 2002jul13 HK Add TTA4 and TTX4
C 2002aug15 HK NWLAT is computed, = 2793
C 2004jul07 HK Move dimension-defining parameters from other commons into KRCCOM
C 2009feb   HK With MAXN1=30, MAXNH=48, MAXN4=37, NWLAT is 7215
C 2010jan12 HK Change to IMPLICIT NONE assumed to be in KRCCOM
C 2014feb25 HK Specify all word lengths as *4
C 2013mar10 HK Make  REAL*8  version    Change name from latcom.inc
C_End _______________________________________________________________________

