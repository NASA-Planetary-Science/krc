      REAL*8 FUNCTION EPRED8 (Y,X,NV,YMIN,YMAX)
C_Title:  EPRED8  Exponential Prediction of numerical iteration  DP version
      IMPLICIT NONE
C_Args:
      REAL*8 Y(*)      ! in.  Uniformly spaced known values.
C                          The last (up to) 3 are used
      REAL*8 X         ! in. Prediction point, spacings FORWARD beyond last known
C                            if <= 0, returns the last valid value.
      INTEGER NV       ! in. Number of valid source points
      REAL*8 YMIN,YMAX ! in. Limits of allowed prediction range
C_Desc
C 3-point prediction; assumes asymptotic exponential form.
C   y = c0 + c1 * exp (c2 * x);  WHERE c2 is <0. or y=c0+c1*r^x where r<1.
C  If not asymptotic (c2 >= 0.) does linear prediction using last 2 points
C  One can call this routine with only 1 or 2 y-values defined,
C   but must remember that the x-origin is at the last known point.
C If NV is 1 or 2, undefined locations may be referenced, but they are not used.
Calls: none
C_Hist Hugh Kieffer  1984jun03
c   98sep03 HK Make linear for  X<XLIM, which allows use with 2 points
C 2005dec28 HK Change to use of IMPLICIT NONE
C 2014mar10 HK Make  REAL*8  version  
C 2018jun23 HK Remove the 2nd dimension on Y. Recode the logic to use  NV
C_End&789012345678901234567890123456789012345678901234567890123456789012_456789

      REAL*8 D1,D2,R,YP
      REAL*8 ONE /1.0D0/

C_End of interface and variable types_________________________________________

C d=y3-y2   dl=y2-y1   r=(y3-y2)/(y2-y1)
C c1 = d/(1 - 1/r)    c0 = y3 - c1    y=y3+[d/(1/r -1)](1-r^x)

      YP = Y(NV)                ! last good point
      IF (X.GT.0.) THEN         ! action only for positive prediction
        D2 = YP - Y(NV-1)       ! slope between last 2 points
 
        IF (NV.GE.3) THEN       ! have 3 valid points
          D1 = Y(NV-1) - Y(NV-2) ! slope between prior 2 points 
          
          IF (D1.NE.0.) THEN    ! division OK
            R = D2/D1           ! ratio of successive changes
          ELSE
            R=ONE               ! flag that should not do 3-point
          ENDIF 
        
          IF (R.GT.0. .AND. R.LT.ONE) THEN ! asymptotic form
            YP = YP + (D2/((ONE/R)-ONE) ) * (ONE -R**X)
          ELSE                  ! linear form using last two points
            YP = YP + X*D2
          ENDIF
        ELSE IF (NV.EQ.2) THEN
          YP=YP+X*D2            ! linear fit to last 2 points
        ENDIF
      ENDIF                     ! uses last point if only 1 point or not forecast.

      EPRED8 = MIN( YMAX, MAX (YP,YMIN)) ! impose bounds
      RETURN
      END
