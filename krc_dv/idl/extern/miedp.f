	SUBROUTINE MIEDP (ANR,ANI,X, QEXT,QSCA,COSA)
C_Titl miedp: stripped down version of mie program from hansen & travis (1974)
C double precision version. does not calculate phase functions.
C_Args
	IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	REAL*8 ANR,ANI	!  I real & imaginary parts of refractive index
	REAL*8 X	!  I particle size parameter; 2*pi*radius/wavelength
	REAL*8 QEXT	!  O extinction coefficent
	REAL*8 QSCA	!  O scattering coefficent
	REAL*8 COSA	!  O phase function coeff; mean cosine ???
C_Desc	based on hansen and travis (1974).
C phase function computation not included.
C_Call 0
C_Hist	82feb--  David a.paige radlib 
C	86jul06  HHKieffer add comments, shorten variable names, and put
C		constants as data variables
C_End
	PARAMETER (MAXZ=20000)	! size of  Z table
	COMPLEX*16 W,W1,W2,A,B,ANC,RRF,RRFX,TC1,TC2,XX,ZZ
	COMPLEX*16 Z(MAXZ)
C next 2 lines use dummy  COMMON to align real and imaginary parts
	COMMON/COMPLX/WR,WI,W1R,W1I,AR,AI,BR,BI
	EQUIVALENCE (W,WR),(W1,W1R),(A,AR),(B,BR)
	DATA PI,TWOPI/3.1415926535897932D0,6.2831853071795865D0/
	DATA ZERO,  ONE,  TWO, THREE, FOUR,   TINY,  TEST
     &     / 0.D0, 1.D0, 2.D0, 3.D0 , 4.D0, 1.0D-30, 1.0D-15/
	DATA NSTART/150/	! minimum summation loop

	ANC=DCMPLX(ANR,-ANI)
	RX=ONE/X		! reciprocal of size parameter
	RRF=ONE/ANC	! complex reciprocal of index
	RRFX=RRF*RX

	RTNRNI = SQRT( ANR**2 + ANI**2)	! magnitude of the index of refraction
	NMX=X*RTNRNI	! # of times in major loop
	IF (NMX.GT.MAXZ) NMX=MAXZ	! insure within storage bounds
	IF(NMX.LT.NSTART) NMX=NSTART	! insure it is at least  NSTART
C 
	N=NMX+NMX/10+1	!  START at  N approx. 1.1*nmx
	ZZ=DCMPLX(ZERO,ZERO)
310		XX=N*RRFX		! recursive,  XX =  N/(X*Complex_index)
		ZZ=XX-ONE/(XX+ZZ)	!  ZZ(N) =  XX-1/(XX+ZZ(N+1))
		N=N-1
		IF (N.GT.NMX) GOTO 310
		Z(N)=ZZ			! for  ZZ(1:NMX)
		IF (N.GT.1) GOTO 310

	T1=COS(X)
	T2=SIN(X)
	W2=DCMPLX(T1,-T2)
	W1=DCMPLX(T2, T1)
	W=RX*W1-W2
	TC1=ZZ*RRF+RX
	TC2=ZZ*ANC+RX
	A=(TC1*WR-W1R)/(TC1*W-W1)
	B=(TC2*WR-W1R)/(TC2*W-W1)

	IF(ABS(AR).LT.TINY) AR=ZERO
	IF(ABS(AI).LT.TINY) AI=ZERO
	IF(ABS(BR).LT.TINY) BR=ZERO
	IF(ABS(BI).LT.TINY) BI=ZERO

	COSB=ZERO
	QEXT=THREE*(AR+BR)
	QSCA=THREE*( AR**2+AI**2 + BR**2+BI**2 )
	PM=ONE
	TM=ONE
C - - - - - - - - - - - - - - - - - - - - - - - - - - major loop
	DO 340 NZX=2,NMX
	TN=NZX
	TP=TN+ONE
	T1=TN+TM
	T2=TN+TP

	A1R=AR		! save prior values
	A1I=AI
	B1R=BR
	B1I=BI
	W2=W1
	W1=W

	W=T1*RX*W1-W2
	ZZ=Z(NZX)
	XX=TN*RX
	TC1=ZZ*RRF+XX
	TC2=ZZ*ANC+XX
	B=(TC2*WR-W1R)/(TC2*W-W1)
	A=(TC1*WR-W1R)/(TC1*W-W1)

	IF(ABS(AR).LT.TINY) AR=ZERO
	IF(ABS(AI).LT.TINY) AI=ZERO
	IF(ABS(BR).LT.TINY) BR=ZERO
	IF(ABS(BI).LT.TINY) BI=ZERO

	TX=T2/(TN*TP)
	PM=-PM
	TX=TM*TP/TN*(A1R*AR + A1I*AI + B1R*BR + B1I*BI)
	COSB=COSB+TX+T1/(TN*TM)*(A1R*B1R + A1I*B1I)
	QEXT=QEXT+T2*(AR+BR)
	TX=T2*( AR**2+AI**2 + BR**2+BI**2 )
	QSCA=QSCA+TX
	TXQSCA=TX/QSCA
	IF (TXQSCA.LT.TEST) GO TO 350
340	TM=TN
C  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
350   CONTINUE
	QEXT=QEXT*TWO /(X*X)
	QSCA=QSCA*TWO /(X*X)
	COSA=COSB*FOUR/(X*X*QSCA)
	RETURN
	END
C
