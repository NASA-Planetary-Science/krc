	REAL FUNCTION ANG360 (DEG)
C_Titl  ANG360 reduces angle (in degrees) to the range 0. to 360.
C_Args
	REAL*4 DEG	! [i] angle, of any sign and magnitude (in degrees)
C_Hist	86jul05 hhkieffer  USGS flagstaff
C_End
	A = MOD(DEG,360.)
	IF (A.LT.0.) A = A + 360.
	ANG360 = A
	RETURN
	END
