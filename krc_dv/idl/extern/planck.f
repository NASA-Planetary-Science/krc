	FUNCTION PLANCK (K,ARG2,ARG3)
C_Title  PLANCK Planck function/inverse; wave-number/length; energy/photon_flux
C_Arguments 
	INTEGER K	! in.  Control code; determines meaning of function 
C 				and of arguments.
	REAL ARG2	! in.  T for K=1 or 3,5, W for K=2,
	REAL ARG3	! in.  W for K=1 or 2, ignored for K=3,5
C				  ARG3 must never be 0.
CC	REAL PLANCK	! f. any of the below 5 functions
C
C		This routine is in effect 5 functions:
C	  Notation: 	T = Temperature in Kelvin
C 			R = Radiance or photon rate   (see K=4)
C			W = Wavelength or wavenumber  (see K=4)
C	K=1: RADIANCE 	  (1,T,W)
C	K=2: TEMPERATURE  (2,R,W)
C       K=3: WAVE_MAX	  (3,T,ignored)
C	K=4: setup_RMIN	  (4,+-rad_type*R_scale,+-wave_type*R_min). see below.
C	K=5: TOTAL_flux   (5,T,ignored) <watt sr^-1 m^-2 >=deflt.
C
C For K=4, sign of ARG2 sets the RADIANCE type, input and output;
C	+ is radiant spectral intensity, or spectral radiance
C		<watt sr^-1 m^-2 / wave>=deflt.
C	- is photon rate, <1.E20 sr^-1 m^-2 / wave>=default
C	   (hemispheric emittance = pi * intensity)
C	The absolute value sets the radiance units, relative to the default;
C		e.g., 1.E-4 would yield <watt sr^-1 cm^-2 / wave>;
C		the default units are obtained by calling with + or - 1.0
C For K=4, sign of ARG3 sets the WAVE type;
C	+ is wavelength <micrometers>
C	- is wavenumber <inverse cm>
C 	The absolute value of ARG3 sets the minimum input radiance to process
C		when called with K=2. The default value is invoked by calling 
C		with the absolute value of ARG3 = 1. The units here are
C		scaled, if the caller has modified the scale.
C	The value used by the routine, in callers units, is returned as 
C		the function when K=4. 
C_Key  IR  BLACKBODY  THERMAL
C_Calls 0
C_Description
C Must call first with  K=4 to set the radiance type and wave type.
C Then can call as  RADIANCE or  TEMPERATURE or  WAVEMAX  or  TOTAL function.
C
C_History 85mar03  Hugh_H_Kieffer  U.S._Geological_Survey_Flagstaff
C	87feb25  HHK revise internal documentation to  NIMS standard
C	87sep16  HHK incorporate default minimum radiance.
C	88mar05  HHK add WAVE_MAX function
C	88may19  HHK add radiance scaling option; clean up a bit.
C	88oct22  HHK add total flux function  (K=5)
C	89dec05  HHK minor comment changes, and 7'th place in  TOTAL
C	91aug14  HHK 1988 physical constants, comment changes
C	94jan24  HHK include  SCALE in coefficient in each function
C_End
	SAVE
C           constants used before 91aug15 from Physics Today, Sept 1974
C h = plancks constant = 6.626176E-34 J sec     J=Joule  s=second  m=meter
C c = speed of light = 2.99792458E8 m sec**-1   K=Kelvin
C k = boltzman constant = 1.380662E-23 J K**-1
C  values from Cohen, E.R., and B.N.Taylor, Physics Today, Aug 1988
	REAL*8 C88 /2.99792458D+08/	! <m sec^-1>
	REAL*8 H88 /6.6260755D-34/	! <J sec>
	REAL*8 K88 /1.380658D-23/	! <J K^-1>
	REAL*8 S88 /5.67051D-8/	! < W M^-2 K^-4> Stephan-Boltzman

C 		Summary of Planck function relations:
C T = temperature <Kelvin>
C L = lambda = wavelength <m>
C U = nu = frequency <1/sec>		U = c/L		U = 100c V
C W = wavelength <micrometers>		W = 1.E6 L 
C V = wavenumber <1/cm>  		V = .01 U/c	V = .01/L
C N : photon number; _U=/frequency   _L=/wavelength  _W=/micron  _V=/inv.cm
C B : spectral intensity; B = hU * N
C  hemispheric emittance = pi * intensity
C Most common formula: pi*B(L) = [2 pi h c^2 / l^5 ] / (e^(hc/kTL)-1)
C  or pi*B = C1 L^-5  / (e^(C2/TL)-1)  C1=2pi hc^2 = 3.7417749E-16 <W m^2>
C the constant C2 == hc/k = 1.438769E-2 meter-kelvin frequently occurs
C  B (W in mu) = C1_W W^-5  / (e^(C2_W/TW)-1)  C1_W=2E24 hc^2 = 1.191043E8
C  C2_W = E6 hc/k = 1.438768E4 <micrometer Kelvin>
C C1 in Planck's Law usually given as 2 pi h c^2 for hemispheric emission;
C   because output here is radiance (not irradiance), factor of pi not used.
C implimentation here computes  N first, then B, so it splits C1 
C   into C1A = 2E18 c * E-20  and  C1B = 1E6 hc * E20
C x = hc/kTL ==  C2/TL == C2_W/TW
C X1 = exp(x) -1.  (also called X in code below)
C if x small, exp(x)-1. ~= x ;  log (1.+x) ~= x
C sigma/pi = 2/15 * pi^4 * k^4 / h^3*c^2 = 2/15 * pi^5 *  C2^-3 * kc
C eq 1. N_L = (2 c / L**4) / X1
C eq 2. N_U = (2 U**2 / c**2) / X1
C eq 3. B_L = (2 h c**2 / L**5) / X1
C eq 4. B_U = (2 h U**3 / c**2) / X1
C {eq 3 or 4} = hU * { eq 1 or 2}.	hU = hc/L = 1.E6 hc/W 
C convert eq 1 to use W:
C   N_W = (L/W) N_L = 1.E-6*(2 c /((1.E-6W)**4)) / X1 = 2E18 c/(W**4 * X1)
C convert eq 2 to use W (not V):
C   N_V = (U/V) N_U = 100 c*(2 {100cV}**2 / c**2) / X1
C       =  200 c*({1E6 c/W}**2 / c**2) / X1 =  2.E14 c / (W**2 * X1)
C When V=W=100,  N_V should equal N_W, which it does.
C
C Total flux: doesn't depend on whether in wavelength or wavenumber mode
C	B = 2/15 pi**4 k**4 / h**3 c**2 * T**4 
C	    2/15 pi**4 C2**-3 kc = 1.8049177E-8 = Stephan_Boltzman constant/pi
C	N = 4*1.202057 k**3 / h**3 c**2 * T**3
C	    4*1.202057 C2**-3 c = 4.839682E+14
C 		^ constant above is Reimann_zeta(3)

	LOGICAL LENGTH	! true for wavelength mode, false for wavenumber
	LOGICAL ENERGY	! true for energy, false for photon_rate

	REAL*4 WAVEMAX(4) /0.90285, 0.50995, 3669.7, 2897.8/ ! for wave_max @ 3
C  WAVEMAX values checked for consistancy with mode 1. 88mar05.
CC	DATA FPBIG,FPSMALL /1.E37,1.E-37/	! floating_point big and small
C get useful limits for evaluating  EXP and ALOG
CC	XMAX = ALOG (FPBIG/2.)	! exp(x) = 1 + x + x^2/2! + x^3/3! + ...
CC	XMIN = 1./"PRECISION"	! log(1-x) =   x - x^2/2  + x^3/3  - ...
	DATA XMAX,XMIN /87.3, 1.E-8/	! Useful limits of EXP(X)

	GOTO (10,20,30,40,50),K


C K=1, F = RADIANCE (1,temperature,wave)

10	W=ARG3			! get W = wave-length or wave-number
	IF (.NOT.LENGTH) W=1.E4/W ! if wavenumber input, convert to wavelength
	X=C2W/(W*ARG2)		! exponent. ARG2 is Temperature
	IF (X.GT.XMAX) GOTO 80	!   negligable, will return 0 radiance
	IF (X.GT.XMIN) X=EXP(X)-1.
	IF (LENGTH) THEN		! get photon number  <1.E20>
		F = C1A/(X * W**4)	!  N_W, from eq 1.
	    ELSE
		F = C1A/(X * W**2)	!  N_V, from eq 2.
	    ENDIF
	IF (ENERGY) F = F * C1B/W	! convert to radiant intensity
	GOTO 9


C K=2, F = TEMPERATURE (2,radiance,wave)

20	W=ARG3			! get W = wave-length or wave-number
	IF (.NOT.LENGTH) W=1.E4/W ! if wavenumber input, convert to wavelength
	R=ARG2
	IF (R.LT.RMIN) R=RMIN
	IF (ENERGY) R = R * W/C1B	! convert to photon number
	IF (LENGTH) THEN		! get x = exp(hc/kWT)-1.
		X = C1A/(R * W**4)	! inverse of Eq 1.
	    ELSE
		X = C1A/(R * W**2)	! inverse of eq 2.
	    ENDIF
	IF (X.GT.XMIN) X = ALOG (X + 1.)
	F = C2W / (X*W)			! get temperature from exponent
	GOTO 9


C K=3, F = WAVE_MAX (3,T,ignored)

30	IF (LENGTH) THEN
		F = WMAX / ARG2		
	    ELSE
		F = ARG2 / WMAX
	    ENDIF
	GOTO 9


C K=4, F = RMIN_setup (4,+-Rad_type*Rad_scale, +-Wave_type*rad_min)

40	ENERGY = ARG2.GE.0.		! set energy / photon flux flag
	RSCALE = ABS(ARG2)		! set radiance scale
	LENGTH = ARG3.GE.0.		! set wavelength / wavenumber flag
	ABS3 = ABS(ARG3)		! save callers minimum radiance
C  RMIN is used to avoid divide-by-0 or overflow for small radiance. 
C	Default values based on largest valid  REAL*4 of FPBIG = 1.7E38, 
C	and minimum  WAVE of 0.1 micrometer or 1E5 inverse centimeter.
C ! set IMODE =	   wavenumber | wavelength
C	! photon >    1		   3
C	! energy >    2            4
C NOTE: C1A divided by E20 and C1B multiplied by E20 to stay in machine range
	C2W = 1.D6*H88*C88/K88	! hc/k <micrometer-Kelvin> 14387.6
	C1B = 1.D26*H88*C88	! 1.E6 hc *E20
	
C  R0 below is default minimum radiance, based on  FPBIG
C and minimum wavelength of 0.1. 
	IF (LENGTH) THEN		! set the scale factor to:
		IMODE = 3
		C1A = 2.D-2*C88		! E18 2 c E-20 (meters/micron)**4 5.99E6
		R0 = 1.E-27		! ~C1A / BIG*Wmin**4
		IF (ENERGY) R0=1.E-24	! ~C1A*C1B / BIG*Wmin**5
	    ELSE			! or
		IMODE = 1
		C1A = 2.D-6*C88	! ??2/(c**2) *(frequency/micron)**2 5.99E2
		R0 = 1.E-35		! C1A / BIG*Wmin**2
		IF (ENERGY) R0 = 1.E-34	! C1A*C1B / BIG*Wmin**3
	    ENDIF
	IF (ENERGY) THEN
		IMODE = IMODE+1
		TOTAL = 1.804918E-8	!  S.I. Stephan_Boltzman / pi
	    ELSE
		TOTAL = 4.839682E-6	!  S.I. * E-20 photon rate
	    ENDIF
	WMAX = WAVEMAX(IMODE)
C SCALE THE CONSTANTS USED IN FREQUENT CALLS
	C1A = C1A*RSCALE
	TOTAL = TOTAL * RSCALE
	RMIN = R0*RSCALE  ! set minimum radiance to default, override only if 
	IF (ABS3.NE.1.) RMIN = MAX(ABS3,RMIN) ! user input is larger.
	F = RMIN		! output rad_min in user units.
	GOTO 9

C  K=5  F = TOTAL (5,T,ignored)

50	 IF (ENERGY) THEN
		F = TOTAL * ARG2**4	!  Stephan_Boltzman law / steradian
	    ELSE
		F = TOTAL * ARG2**3
	    ENDIF
	GOTO 9

80	F=0.
C
9	PLANCK=F
	RETURN
	END
