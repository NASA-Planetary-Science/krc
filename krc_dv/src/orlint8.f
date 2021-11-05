      SUBROUTINE ORLINT8 (NIN,XIN,YIN,NOUT,XX,YY)
C_Title  ORLINT8 Linear interpolation in ordered set for ordered request
      IMPLICIT  NONE
      INTEGER NIN       ! in. number of points in xin and yin.
                        !     Must be 2 or more
      REAL*8 XIN(NIN)   ! IN. INDEP POINTS. MUST BE INCREASING. Else will stop
      REAL*8 YIN(NIN)   ! IN. DEPEND. VALUES
      INTEGER NOUT      ! IN. NUMBER OF OUTPUT POINTS.  Any positive 
                        !     value up to smaller of  XX and  YY
      REAL*8 XX(NOUT)   ! IN. DESIRED POINTS. MUST BE INCREASING. Else will stop
                        !    MAY BE OUTSIDE RANGE OF XIN
      REAL*8 YY(NOUT)   ! OUT. LINEAR INTERPOLATION RESULT
C_Desc  Checks for the ordering of input and output x, stops if violated.
C_Hist Hugh Kieffer 2018feb01
      INTEGER J,LO,HI
      REAL*8  SLOPE, XIHI, XPRIOR

      LO=0                      ! LOWER INDEX OF INTERPOLATION INTERVAL
      XIHI=-1.d300              ! below any reasonable first data point
      XPRIOR=XIHI               ! below any reasonable first request point
      DO J=1,NOUT 
        IF (XX(J).LT.XPRIOR) STOP  !  XX must not decrease 
 10     IF (XX(J).GE.XIHI .AND. LO.LT.NIN-1 ) THEN ! Use NEXT HIGHER INTERVAL
          LO=LO+1               ! NEXT INTERVAL
          HI=LO+1
          IF (XIN(HI).LE.XIHI) STOP !  XIN must increase
          XIHI=XIN(HI)          ! top of this interval
          SLOPE=(YIN(HI)-YIN(LO))/(XIHI-XIN(LO))
          GOTO 10               ! check if now in right interval
C     else, must stick with final input interval
        ENDIF
        YY(J)= YIN(LO)+ (XX(J)-XIN(LO))*SLOPE
        XPRIOR=XX(J)
      ENDDO

      RETURN
      END
