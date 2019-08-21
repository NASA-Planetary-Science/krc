	REAL FUNCTION RNDEX (XIN,XX,N)
C_Titl  RNDEX  Finds floating-point index of input argument within a R*4 array
	IMPLICIT  NONE
C Function safely inside by epsilon  if XIN  not within  XX limits
C_Args
	REAL*4  XIN	! [i] value to be located within the array
	REAL*4  XX(1)	! [i] array to be searched; should be monotonic;
C				but can be of either slope.
	INTEGER  N	! [i] size of array
C_Desc  Uses binary search, and then linear interpolation within last interval.
C_Calls  0
C_Hist  83Dec05  Hugh_H_Kieffer
C	87dec15  HHK convert from exhaustive search to binary search
C 2011jul31 HK Replace search code with NumRec LOCATE algorithm
C 2011oct23 HK Fix 4 bugs
C_End
	REAL F
	INTEGER  JL,JM,JU
	LOGICAL INC

C NumRec LOCATE  algorithm: 	
	JL=0			! lower test point index
	JU=N+1			! upper "
	INC = XX(N).GT.XX(1)	! true if table is increasing
 10	IF (JU-JL.GT.1) THEN	! need to narrow range further
	   JM=(JU+JL)/2		! middle point
	   IF (INC .EQV. (XIN.GT.XX(JM))) THEN 
	      JL=JM
	   ELSE
	      JU=JM
	   ENDIF
	   GOTO 10
	ENDIF
C at this point, JL is the index at lower end of interval containing X
C JL = 0 or N means X is outside the table
	IF (JL.EQ.0) THEN	! request below end of table
	   RNDEX=1.00001	! return safely inside lower end
	   GOTO 9
	ELSEIF (JL.EQ.N) THEN	! request above end of table
	   RNDEX=FLOAT(N)-.0001 ! return safely below upper end
	   GOTO 9
	ELSE
	   F = (XIN-XX(JL))/(XX(JU)-XX(JL)) ! fractional way into the interval
	   RNDEX = FLOAT(JL) + F ! real index
	ENDIF

9	RETURN
	END
