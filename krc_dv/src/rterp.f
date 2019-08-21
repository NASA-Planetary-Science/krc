	REAL FUNCTION RTERP (A,XI)
C_Title  RTERP  linear interpolation of array at a real index
C_Args
	REAL*4 A(*)	! in. array which will be interpolated
	REAL*4 XI	! in. index at which interpolated value desired
C_Keys  MATH  INTERPOLATION
C_Desc	simple linear interpolation between two points of array.
C  The routine  RNDEX could be called first to determine  XI.
C if  XI is negative, assumes that point is outside table, and that 
C   this represents nearest endpoint, whose value is returned.
C  RNDEX could return  XI as the index of exactly the last point in a 
C table, in which case formula below will use undefined next storage location;
C  {A(I+1)}, but the coefficent  (XI-FLOAT(I)) will be zero, so should still
C get the right result.
C_Lims	does not check for index beyond the size of array.
C_Call  0
C_Hist	83jul06  Hugh_Kieffer_U.S.G.S._Flagstaff  ORIGINAL_VERSION
C	86aug08  HHK endpoint handling and documentation added
C	87dec15  HHK explicit  FLOAT added
C 	91sep07  HHK comment about last point added.
C_End
	IF (XI.GE.1.0) THEN	! expect to be within table
		I= XI		! index of lower point
		RTERP = A(I) + (XI-FLOAT(I))*(A(I+1)-A(I))
	    ELSE		! not within table
		IF (XI.GE.0.) I=1	! would not occur from  RNDEX
		IF (XI.LT.0.) I = IFIX (- XI +0.001)
		RTERP = A(I)	! use nearest endpoint
	  ENDIF
	RETURN
	END
