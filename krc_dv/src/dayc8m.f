C_Titl  daycom8.f   common for layer and time items in  KRC
      INTEGER*4 NWDAY
      PARAMETER (NWDAY = 5*MAXN1 + MAXN1P + (5+MAXN1)*MAXN3
     &  + 8*MAXN2 + 2*MAXNH + MAXBOT/2) ! size of this common in real words
C_Desc
C  ASOL &  ADGR are set in  TLATS, the rest are set in  TDAY
      REAL*8 XCEN(MAXN1)        ! Depth at layer centers [m]
     &, SCONVG(MAXN1)   ! Classical convergence factor for each layer
     &, BLAY(MAXN1P)    ! Layer thicknesses [m]
     &, TMIN(MAXN1)     ! Minimum layer temperatures of day on the hour
     &, TMAX(MAXN1)     ! Maximum layer temperatures of day on the hour
     &, TTJ(MAXN1P)     ! Layer temperatures (TTJ(1) is surface temperature)    
     &, TT1(MAXN1,MAXN3)! Temperatures at start of day for each layer and day 
     &, TTS(MAXN3)      ! Mean daily surface temperatures                     
     &, TTB(MAXN3)      ! Mean daily bottom temperatures                      
     &, TTA(MAXN3)      ! End-of-Day Atmospheric temperatures
     &, DTMJ(MAXN3)     ! RMS daily temperature change
     &, FRO(MAXN3)      ! Daily frost amounts. [kg/m^2]                    
     &, ASOL(MAXN2)     ! Direct solar flux on sloped surface at each time of day
     &, ADGR(MAXN2)     ! Atm. solar heating at each time of day
     &, ALBJ(MAXN2)          ! hemispherical albedo at each time of day 
     &, SOLDIF(MAXN2)  ! Solar diffuse (with bounce) insolation each time  W/m^2
     &, FINSOL(MAXN2)      ! eclipse insolation factor
     &, PLANH(MAXN2)         ! planetary thermal load  W/m^2 
     &, PLANV(MAXN2)         ! planetary visual (solar) load  W/m^2  
     &, TOUT(MAXN2)     ! Surface temperatures of solution at each time of day
     &, TSFH(MAXNH)     ! Hourly surface temperatures at solution
     &, TPFH(MAXNH)     ! Hourly planetary temperatures at solution
      INTEGER*4 N1K(MAXBOT)     ! Binary time division layers

      COMMON /DAYCOM/ XCEN,SCONVG, BLAY, TMIN,TMAX, TTJ, TT1
     &, TTS,TTB,TTA,DTMJ,FRO, ASOL,ADGR,ALBJ,SOLDIF,FINSOL
     &, PLANH,PLANV,TOUT, TSFH,TPFH, N1K

C_Hist  84jun15  Hugh_H_Kieffer  97feb11  HHK add  ADGR  
C   97mar03 HK correct  NWDAY       97sep08  HHK add  TPFH
C 2002jul12 HK Add TTA, ADGR was down-going atmospheric  IR radiance
C 2002aug15 HK NWDAY is computed, =1951
c 2002oct30 HK set N2 to 8*384 as trial  NWday=10015
C 2004jul07 HK Move dimension-defining parameters from other commons into KRCCOM
C 2008oct02 HK Move MAXBOT and MAXN1P from here to KRCCOM
C 2008nov13 HK Change names: X->XCEN  T->TTJ  TT->TT1  TLAY->BLAY
C 2010jan12 HK Change to IMPLICIT NONE assumed in krccom
C 2014feb25 HK Specify all word lengths as *4
C 2013mar10 HK Make  REAL*8  version    Change name from daycom.inc
C 2018jan21 HK Move  ALBJ,SOLDIF,FINSOL,PLANH,PLANV from  HATCOM  to DAYCOM
C_End __________________________________________________________________________

