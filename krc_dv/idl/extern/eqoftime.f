	REAL FUNCTION EQOFTIME (DJ, DEC,AU)
C_Title  EQOFTIME equation of time
C_Args
	REAL*4 DJ	!in. julian day from  J2000.0, = jd - 2451545.5
	REAL*4 DEC	!out. solar declination <deg>
	REAL*4 AU	!out. sun-earth distance  <A.U.>
CC	REAL*4 EQOFTIME !f. apparent time minus real time <minutes>
C_Keys  ORBITAL_GEOMETRY  TIME
C_Desc
C taken from the astronomical almanac, 1984, page  C24.
C precision over 1950 to 2050:  EQOFTIME, 0.1 min; solar position, .01 deg.
C_Hist	92jan11  Hugh_H_Kieffer  U.S.G.S._Flagstaff   original_version
C_End&___1_________2_________3_________4_________5_________6_________.72
CC	DATA	SINO,COSO / .39777249 , .91748408 / ! sin, cos of obliquity
	REAL*4 R2D /57.29578/	  ! degrees in one radian
	REAL*4 D2R /1.7453293E-2/ ! radians pre degree
D	IP = 6	! printer unit
CC	AN = DJ - 2451545.0	! n = # days from  J2000
	AN = DJ
	SLON = ANG360 (280.460 + 0.9856474*AN)	!  L = mean longitude of sun, deg
	G    = D2R*(357.528 + 0.9856003*AN)	! g = mean anomoly, radians 
	ELON = SLON + 1.915*SIN (G) + 0.020*SIN (2.*G) ! lambda =eclipt lon
D	ELON1 = ELON
	    IF (ELON.GT.360.) THEN
		ELON=ELON-360.	! could be as large as 361.935
	    ELSEIF (ELON. LT.0.) THEN
		ELON=ELON+360.	! or as small as -1.935
	    ENDIF
	    ELONR=D2R*ELON
C values for obliq computed from linear terms as given on page  S21
	OBLQ = D2R*(23.439291 - 3.5628E-7 * AN) ! epsilon = obliquity of ecliptic

	RA = r2d*ATAN (COS (OBLQ) * TAN (ELONR))	! alpha, right ascension
D	RA1 = RA
C  RA will be +- 90.,  need to put into same quadrant as lambda
	    IF (ELON.GT.270.) THEN	!  4th quadrent,  RA will be negative
		RA = RA + 360.
	    ELSEIF (ELON.GT.90.) THEN	! in 2nd or 3rd quadrents
		RA = RA +180.
	    ENDIF

	DEC = r2d*ASIN (SIN (OBLQ) * SIN (ELON) )! declination, degrees
	AU = 1.0014 - 0.01671*COS (G) - 1.4E-1*COS (2.*G)

	DELA = ANG360 (SLON-RA)	! this should be no larger than about 4 deg.
D	DELA1 = DELA
	    IF (DELA.GT. 180.) THEN
		DELA = DELA-360.
	    ELSEIF (DELA.LT.-180.) THEN
		DELA = DELA+360.
	    ENDIF
	EQOFTIME= 4.*DELA
D 	WRITE (IP,33) SLON,ELON1,ELON,RA1,RA,DELA1,DELA
D 33	FORMAT (1X,7F10.2)
	RETURN
	END
