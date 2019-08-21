      SUBROUTINE FL1 (A,B,NIN,KIN,G,T)
C_TITL  FL1 General one-dimensional filter
C_ARGS
	REAL A(*)   ! [I] Input array	
	REAL B(*)   ! [O] Output array, must not overlap  A(*).
C			  B(1) set to -1.E20 if formal error occurs.
	INTEGER NIN ! [I] Input # of points in array
	INTEGER KIN ! [I] Input filter wing size. Filter width is  2*KIN+1 
	REAL G	    ! [I] Input wing shape
C		       + = gaussian sigma
C		   or 0. = box filter
C		   or -1.= triangular filter  (KIN'th point on wing has 
C			   weight 0)
C		   or else assumes that  T contains the filter half-shape
C			upon input.
	REAL T(*)   ! [I] buffer for half weighting function.Size >=  KIN. 
C			  If  G<-1,  T must be input as filter wing shape
C                         (excluding central point) upon input. I.e., 
C			   T(1) = point next to central point
C_DESC  Filter one dimensional array with box, triangle, gaussian or user-
C  supplied filter.
C Tests for valid number of points and valid filter-width.
C Filter may be wider than the data array.
C A user-supplied filter wing should be normalized to unity central weight.
C Constructs filter half-wing as a table.
C Algorithm not especially fast, however, treats end-points strictly correct.
C The filter is not reflected off a boundary, but rather any portion of the 
C   filter which does not overlap the input array is ignored.
C_CALLS 0
C_HIST	82Nov27  Hugh_H_Kieffer  U.S.G.S_Flagstaff
C	88mar05  HHK revise toward structured code, revise documentation
C       98may24  HHk repalce  SNGL with  REAL
C_END       	

	K=KIN
	N=NIN
	IF (N.LT.1 .OR. K.LT.1 ) GOTO 80 	! test for formal errors

C algorithm would work with any symmetric weighting function of 
C   unit central weight.

C construct one wing of weighting function
	IF (G.GT.0.) THEN		! Gaussian
		DO I=1,K
			S=(REAL(I)/G)**2/2.
			T(I)=EXP(-S)
			ENDDO
	    ELSEIF (G.EQ.0.) THEN	! Box filter
		DO I=1,K
			T(I)=1.
			ENDDO
	    ELSEIF (G.EQ.-1.) THEN	! Triangular filter
		IF (K.LT.2) GOTO 80
		W=REAL(K)
		K=K-1		! avoid filter ends of weight zero
		DO I=1,K
			T(I)=1.-REAL(I)/W
			ENDDO
	    ENDIF
C point loop. 
	DO I=1,N
		S=A(I)		! start with central point
		W=1.		! given weight of one.
		DO L=1,K		! proceed out wings
			J=I-L			! left wing
			IF (J.GE.1) THEN
				S=S+T(L)*A(J)
				W=W+T(L)
			    ENDIF
			J=I+L			! right wing
			IF (J.LE.N) THEN
				S=S+T(L)*A(J)
				W=W+T(L)
			    ENDIF
			ENDDO
		B(I)=S/W
		ENDDO
9	RETURN

80	B(1)=-1.E20		! indicates formal error
	GOTO 9
	END
