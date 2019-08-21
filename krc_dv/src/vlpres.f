      REAL FUNCTION VLPRES (KODE, DATE)
C_Titl  VLPRES  Viking lander pressure curves  Using J2000 dates
      IMPLICIT NONE
C_Args
      INTEGER KODE      !in.  1=VL1_yr1  2=VL1_yr2  3=VL2  4=normalized average
      REAL DATE !in. date in julian days, offset from J2000.0   Was from 2440000
C_Desc
C  Derived from the coefficents in:  JGR vol 98,  E6, pp 10,973-971
C    provided by  Neal  Johnson.
C relation is  P = pmean*sum [sin (i*2*pi*frac_year + phase(i)*{pi/180})]
C where frac_year is the fraction of a martian year from base time of equations;
C  VL1 sol 405 &  VL2 sol 360.50865.
C  I compute these as  JD 2440000 + 3395.45 & 3395.53 respectively.
C this is stated to be  L-sub-s 330.2 in article.
C upon first call, each set of coefficents is normalized 
C to its mean pressure, and the 3 curved averaged.
C  If called with invalid  KODE, 4 will be used.
C ---------
C  The following values from  Neal: 
C  Lander 1, local midnight, sol 1.0 is at julian date =  2442980.346866330
C == gmt time    = 1976 202 20:19:29.25     20 july      
C == epoch & gcsc=   2        4243427  == llt = 1  0: 0: 0.00
C == lsubs       =     97.19599
C  Lander 2, local midnight, sol 1 is at julian date =  2443026.061346330
C == gmt time    = 1976 248 13:28:20.32      4 september 
C == epoch & gcsc=   2        6533130  == llt =  1  0: 0: 0.00
C == lsubs       =    117.99265
C_Hist 97feb11  Hugh_Kieffer original version
C 99nov18  HHK  add  SAVE statement
C 2010jan11 HK Go to implicit none
C 2014apr27 HK Adjust dates to J2000 system; should have been done 2013jan30
C               and untabify
C_End6789012345678901234567890123456789012345678901234567890123456789012_4567890
      REAL PMEAN  (4) ! mean pressure (4th is normalized to 1.)
      REAL AMP  (5,4) ! amplitude of harmonic term
      REAL PHASE(5,4) ! phase of harmonic term

      REAL YEARM / 686.9957/ ! was 686.98 /
      REAL DJ0 / 94.438 / ! was 3395.49 Julian date for L-sub-s of start of curve
C        this is average of  VL1 and  VL2 origins, near L_s 330.
CC    REAL DJ0 / 10265.0 /  L_S 330. OCCURS ON  JD 2450265.0  = 1996 Jun 30.5
C this is 10.000 martian years later than start of pressure curve
C
C Want DJ0 in J2000 to to be a integral multiple of modern YEARM from the 
C 2440000 base DJ0 of 3395.49
C 2440000+3395.49 = 2451545 + X +-N*686.9957    Choose N=12, get X= 94.438355
C Check, IDL: LSAM(X) gives 330.23895     Lsubs(x) gives 330.24139 
      LOGICAL LFIRST /.TRUE./
      REAL D2R,DJ,FRAC,FYEAR,PI,PIF,RI,SUM,SUMA,SUMP
      INTEGER I,J

      DATA PMEAN / 7.936, 7.942, 8.663, 1./

      DATA AMP / 0.661, 0.574, 0.113, 0.065, 0.014
     &         , 0.658, 0.557, 0.096, 0.056, 0.015
     &         , 0.798, 0.613, 0.114, 0.063, 0.018
     & , 5*0./

      DATA PHASE /91.96, -129.23, -66.20,  -4.04, 25.60 ! in degrees
     &         ,  91.37, -131.80, -75.58,  -8.78, 24.22
     &         ,  93.59, -131.37, -67.50, -17.19, 98.84
     & , 5*0./

      SAVE LFIRST,AMP,PHASE,D2R ! insure these are defined after first call


      IF (LFIRST) THEN          ! form normalized averages
        DO I = 1,5                ! do each period
          SUMA = 0.
          SUMP = 0.
          DO J = 1,3              ! for each input set
            SUMA = SUMA+AMP(I,J)/PMEAN(J)
            SUMP = SUMP+PHASE(I,J)
          ENDDO
          AMP(I,4) = SUMA/3.      ! store them in 4th array
          PHASE(I,4) = SUMP/3.
        ENDDO
        PI = ACOS(-1.) ! pi to machine precision
        D2R = PI/180.
        LFIRST = .FALSE.
      ENDIF

      J = KODE                    ! transfer to local
      IF (J.LT.1 .OR. J.GT.4) J = 4 ! and insure valid value

C find positive year fraction
      DJ = DATE                   ! transfer input date to local variable
      IF (DJ.LT.DJ0) THEN       ! if neg offset, add years to make positive
        FRAC = (DJ0-DJ)/YEARM     ! determine how many years to add
        DJ = DJ+INT(FRAC+1.)*YEARM
      ENDIF
      FYEAR = AMOD((DJ-DJ0)/YEARM,1.) ! positive fraction of year 
      PIF = 2.*PI*FYEAR         ! 2 pi fyear; constant factor in equation

C evalute the formula
      SUM = PMEAN(J)
      DO I=1,5
        RI = REAL(I)
        SUM = SUM + AMP(I,J) * SIN (RI*PIF+ PHASE(I,J)*D2R)
      ENDDO
      
      VLPRES = SUM ! transfer to function
      RETURN
      END
