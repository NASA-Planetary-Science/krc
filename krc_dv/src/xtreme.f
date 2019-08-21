
	SUBROUTINE XTREME (AA,ID1,N, XMIN,XMAX)
C_Title  XTREME Find extreme range of a real array
C_Arguments
	REAL AA(ID1,*) ! [I] input array
	INTEGER ID1    ! [I] Spacing of elements (first dimension of A), 
C				should be 1 for singly dimensioned array
	INTEGER N      ! [I] number of element in AA
 	REAL XMIN      ! [O] minimum value in AA
	REAL XMAX      ! [O] maxmum value in AA
C_Calls 0
C_History  86MAR20  Hugh_H_Kieffer original version
C_end
C

	XMIN = AA(1,1)
	XMAX = XMIN
	IF (N.LT.2) GOTO 9
	DO I=2,N
	    A=AA(1,I)
	    IF (A.LT.XMIN) THEN
		XMIN = A
	    ELSEIF (A.GT.XMAX) THEN
		XMAX = A
	    ENDIF
	  ENDDO
9	RETURN
	END
