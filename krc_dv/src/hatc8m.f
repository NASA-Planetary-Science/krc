C_Titl  hatc8m.f   HATCOM: common to store post-2003 items in  KRC
      REAL*8 HEATMM(MAXN4)   ! daily average surface heat flow,  <W m^-2>
     &, TEXTRA(MAXN4,2)      ! Extrapolation in surface/bottom temperature
     &, TAF(MAXNH,MAXN4)     ! final hourly atmosphere temperature, not predicted
     &, TOFALB(MAXNH,MAXN4)  ! hourly top-of-atm albedo, not predicted
     &, DOWNVIS(MAXNH,MAXN4) ! hourly net downward solar flux
     &, DOWNIR(MAXNH,MAXN4)  ! hourly net downward thermal flux
     &, DUM8M(NUMH4) ! MAXNH*MAXN4 dummy, can be used for arrays up to this size 
     &, HEAT1M               ! Mean upward heat flow into surface on last day
     &, SALB                 ! spherical albedo of the soil
      REAL*8 FARTS(MAXNH,MAXN4,2) ! far-field Tsurf/Tatm for current season
     &, FARAD(MAXFF) ! far-field radiance for every time-step at current latitude
     &, HARTA(MAXFF) ! flat-case Tatm  for every time-step at current latitude
C     &, TMN4Y(MAXN6,MAXN1,MAXN4)! midnight temperatures (year,layer,lat.)
      REAL*8 PARC(12) ! Eclipse parameters    First value non-pos means turn off.
      REAL*8 PARW(7)  ! Flux load from the planet. First value neg means turn off
      INTEGER*4 NLAD  ! latitude for high-resolution output <1 means none
      INTEGER*4 I4XT  ! 4-byte fill to make common size multiple of 8 bytes
C  Note, order in commom based on size, not the same order as above.
      INTEGER NWHAT    ! size of this common in  R*8 words
      PARAMETER (NWHAT= (7*MAXNH+3)*MAXN4+2*MAXFF+22)  
      COMMON /HATCOM/ FARTS,TAF,TOFALB,DOWNVIS,DOWNIR,DUM8M !  MAXNH*MAXN4*(2+4)
     & ,TEXTRA,HEATMM                    !  MAXN4*(2+1) 
     & ,FARAD,HARTA                      !  MAXFF*2
     & ,PARC,PARW, HEAT1M,SALB, NLAD,I4XT !  12+7+2+1 =22 
C_Notes
C 2016jul07 ALBJ and SOLDIF should move to daycom, SALB should move to krccom.

C_Desc Designed for seasonal heatflow into annual frost
C_Hist 2004jul05 Hugh Kieffer
C 2004Oct05 HK Add DOWNVIS and DOWNIR
C 2009apr22 HK Add TMN4Y
C 2010jan12 HK Change to IMPLICIT NONE assumed in krccom
C 2014feb25 HK Specify all word lengths as *4
C 2014mar10 HK Make  REAL*8  version      Change name from hatcom.inc 
C 2016may08 HK Add items for flat far field,  move HEAT1M  -from first 
C 2016jul07 HK Include  ALBJ and SOLDIF to handle non-Lambertian albedo
C 2017mar12 HK Include eclipse and planet fluxes. As of 2017mar24, FINSOL is in
C  common to avoid individual copies in TLATS and TFINE, not for transfer.
C  It has different uses in these two routines.
C 2017apr06 HK Add NLAD (and i4xt).  Order common by variable size
C 2018jan21 HK Increase size of PARC by 1 to accomodate lat-dep. bias
C 2018jan21 HK Move  ALBJ,SOLDIF,FINSOL,PLANH,PLANV from  HATCOM  to DAYCOM
C 2018oct17 HK Add DUMM8, used by TSEAS and TLATS
C_End __________________________________________________________________________

