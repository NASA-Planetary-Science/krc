      REAL FUNCTION FINTERP (XIN,XX,N,YY)
C       _Titl  FINTERP  Floating-point interpolation of Y for monotonic X
      IMPLICIT  NONE
C       Function will be Y at X limits if  XIN  not within  XX limits
C       _Args
      REAL*4  XIN           ! [i] value to be located within the array
      REAL*4  XX(1)         ! [i] array to be searched; should be monotonic;
C       but can be of either slope.
      REAL*4  YY(1)         ! [i] array of dependant values
      INTEGER  N            ! [i] size of array
C       _Desc  Uses binary search, and then linear interpolation within last interval.
C       _Calls  0
C       _Hist  2011jul31 HK  Use search code with NumRec LOCATE algorithm
C       _End
      REAL F
      INTEGER  JL,JM,JU
      LOGICAL INC

C       NumRec LOCATE  algorithm:     
      JL=0                ! lower test point index
      JU=N+1              ! upper "
      INC = XX(N).GT.XX(1)    ! true if table is increasing
 10     IF (JU-JL.GT.1) THEN    ! need to narrow range further
        JM=(JU+JL)/2        ! middle point
        IF (INC .EQV. (XIN.GT.XX(JM))) THEN 
          JL=JM
        ELSE
          JU=JM
        ENDIF
        GOTO 10
      ENDIF
C       at this point, JL is the index at lower end of interval containing X
C       JL = 0 or N means X is outside the table
      IF (JL.EQ.0) THEN       ! request below end of table
        FINTERP=YY(1)       ! return lower end
        GOTO 9
      ELSEIF (JL.EQ.N) THEN   ! request above end of table
        FINTERP=YY(N)       ! return upper end
        GOTO 9
      ELSE
        F = (XIN-XX(JL))/(XX(JU)-XX(JL)) ! fractional way into the interval
        FINTERP = YY(JL)+F*(YY(JU)-YY(JL)) ! linear interpolation
      ENDIF

 9      RETURN
      END
