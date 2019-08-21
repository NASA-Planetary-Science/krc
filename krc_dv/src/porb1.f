	SUBROUTINE PORB1 (TITLE)
C_Titl  PROB1  Read orbital elements from disk file 
C  Computes orbital elements and rotation matrices of date into common
C_Calls:  OBLIX, vector pkg
C_HIST  Hugh Kieffer  mid-1975  78may28  79may11. vax version 84may24  85jan13
C 1985sep13 HK Combine all PORBEx into one, Revise called argument order
C 2009dec14 HK Minor changes to allow 4th file code
C 2009dec24 HK Change PERIOD and PI names to agree with revised porbcm
C 2013feb05 HK Go from 1950 to J2000 and remove reference to obsolete data files
C_End&___1_________2_________3_________4_________5_________6_________.72

	INCLUDE 'porbcm.inc' ! has  IMPLICIT NONE
C_Args 
	CHARACTER*(*) TITLE  !out. object name

	INTEGER I,INCODE,IPLAN
	REAL PIO2		! constants
C	REAL TWOPI		! constants
	REAL PERIOD
	REAL OBLIP		! functions
C
	PIO2=PICON/2.		! pi over 2
C	TWOPI=2.*PICON
C
1	WRITE (IOS,*) ' ?* SOURCE DATA: '
     & ,'1=Planets, 3=minor [4=comets] -=quit'
	READ (IOK,*,END=9,ERR=1) INCODE
	IF (INCODE.LT.1) RETURN
11	WRITE (IOS,*) ' ?* WHICH item in file.'
	READ (IOK,*, END=9,ERR=11) IPLAN
	IF (INCODE.EQ.1) THEN
 2	   WRITE (IOS,3)
 3	   FORMAT (' ?* EPOCH desired as centuries after 2000.0')
	   READ (IOK,*,ERR=2,END=9)TC
	ENDIF
	CALL PORBEL (IOP,IOD,INCODE,IPLAN,TC, ODE,CLIN,ARGP
     &,ECC,PERIOD,SJA,SIDAY,ZFQB,ZFQA,TJO,TITLE)
	OPERIOD=PERIOD		! transfer to common
	WRITE (IOS,*)TITLE
	WRITE (IOP,*)' INPUT FILE WAS: ',TITLE 
C	OBL= .40931975	        ! earths obliquity in radians
	OBL= .409092601		! ecliptic obliquity in radians IAU2009
	CQA=0.			! reference star Right Ascension
	CQB=0.			!  " declination
C----------------------------Calculations-----------------------------
       PLANUM=100.*INCODE+IPLAN ! load object number into common
C get planets pole in ecliptic coord., and location of planets vernal equinox
	CALL OBLIQ (ZFQA,ZFQB,CLIN,ODE,OBL, ZFEB,ZFEC,XFEXB,ARGV)
C get obliquity: planets pole relative to planet orbit normal
	BLIP=OBLIP(ZFQB,ZFQA,ODE,CLIN,.FALSE.) ! all angles all radians
C set offset for l-sub-s
	SLP=ARGP-ARGV ! ArgPeriapsis -  (from node to planets vernal equinox)
C compute:  FQ =to planet equator-ecliptic from earth equitorial
	CALL ROTDIA (1., FQ)
	CALL ROTAX  (1,OBL, FQ)
	CALL ROTAX  (3,ZFEB+PIO2, FQ)
	CALL ROTAX  (1,ZFEC, FQ)
	CALL ROTAX  (3,XFEXB+PICON, FQ)
C  EO = from planet orbit to heliocentric ecliptic
	CALL ROTDIA (1., EO)
	CALL ROTAX  (3,-ARGP, EO)
	CALL ROTAX  (1,-CLIN, EO)
	CALL ROTAX  (3,-ODE, EO)
C switch to sun around planet
	DO 80 I=1,9
80		HO(I)=-EO(I)
C  HO = Form orbital to heliocentric planet fixed
	CALL ROTAX  (3,ZFEB+PIO2, HO)
	CALL ROTAX  (1,ZFEC, HO)
	CALL ROTAX  (3,XFEXB+PICON, HO)
9	RETURN
	END
