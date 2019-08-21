      SUBROUTINE SIGMA8 (BUF,N, AVE,SIG)
C_TITLE  SIGMA8  Compute mean and standard deviation of real*8 array 
      IMPLICIT NONE
C_ARGS
      REAL*8 BUF(N)   ! [In] array
      INTEGER N       ! [In]  number of items in BUF
      REAL*8 AVE      ! [Out] average value of input array
      REAL*8 SIG      ! [Out] standard deviation of input array
C_Hist    2016aug18 Convert sigma.f from R*4 to R*8 
C_END
    
      INTEGER I                 ! index
      REAL*8 P,X                ! number of points, any value
      REAL*8 S,SS               ! sum and sum of squares

      SIG=0.D0                  ! set here in case N<2
      IF (N.LT.1) THEN          ! not enough points 
        AVE=0.D0
        RETURN
      ENDIF
      P=N                       ! need real number_of_points for formulas
      S=0.D0                    ! initialise the sums
      SS=0.D0
      DO I=1,N
        X=BUF(I)
        S=S+X                   ! sum the values
        SS=SS+X*X               ! sum the values-squared
      ENDDO
      AVE=S/P                   ! average
      IF (N.GT.1) SIG=SQRT((SS-2.D0*AVE*S+P*AVE*AVE)/(P-1.D0)) ! standard dev.
      RETURN
      END
