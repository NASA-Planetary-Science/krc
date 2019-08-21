      SUBROUTINE PORBEL (IOP,IOD,IFILE,IPLAN,TCEN, RODE,CLIN,ARGP
     &,XECC,PERIOD,SJA,SIDAY,PRA,PDEC,TJP,TITLE)
C_Title  PORBEL read planetary orbital element file, compute basic constants
      IMPLICIT NONE
C_Args
      INTEGER IOP               !i logical unit for report, if not=0.
      INTEGER IOD               !i logical unit number to use for input data file
      INTEGER IFILE             !i which type of input file
      INTEGER IPLAN             !i body in file; for planets, first 9 are  
C                               number counting out from Sun
      REAL*8 TCEN !i request time in Julian centuries (=36525 days) from J2000.0
C Next 3 items are in J2000 ecliptic system (default)
      REAL*8 RODE               !o longitude of ascending node <radians>
      REAL*8 CLIN               !o inclination <radians>
      REAL*8 ARGP               !o argument of periapsis <radians>
      REAL*8 XECC               !o eccentricity
      REAL*8 PERIOD             !o orbital period in days
      REAL*8 SJA                !o semi-major axis in astronomical units
      REAL*8 SIDAY              !o length of siderial day in hours
C Next 2 items are in J2000 equatorial system (default)
      REAL*8 PRA                !o right ascension of pole <radians>
      REAL*8 PDEC               !o declination of pole <radians>
      REAL*8 TJP                !o julian day (offset from J2000.0) of perihelion
      CHARACTER*(*) TITLE       !o planet name
C_Desc  
C Planetary orbital elements referred to mean ecliptic and equinox of J2000
C Reads files in  degrees, returns radians
C read a file based on:
C  1:  http://iau-comm4.jpl.nasa.gov/XSChap8.pdf Table 8.10.2
C  2:  IAU Working Group on Cartographic Coordinates and Rotational Elements
C  3:  Minor planets, several with spin axis defined
C  4:  Asteroids, minor-planets, comets based on JPL Horizon cut/paste
C  5:  Comet elements [from Ted Bowell 1985sep07]
C  6:  ExoPlanets
C
C Longitude of the node, omega. From vernal equinox in plane of reference to 
C       the ascending node
C Argument of perihelion, w. From the ascending node to periapsis in the 
C       orbital plane
C Longitude of perihelion, varpi == omega+w
C Mean anomaly, M. From perapsis to the mean object in the 
C       orbit plane. M = 2 pi (time from periapsis / period)
C Mean Longitude, L. == varpi+M
C Longitude at epoch =omega+w+v ??
C Eccentric anomaly, E. M=E - e sin E  Keplers equation; where e=eccentricity 
C      and angles are in radians
C_File  IOD = orbital elements file, opened and closed
C_Calls  COCOCM  COCOMC  ROTV  ROTVEC  UPCASE  YMD2J2  
C_Liens No END or ERROR test for reads within a asteroid or comet set
C_Hist  85feb00  Hugh_H_Kieffer  U.S.G.S._Flagstaff   original_version
C       89sep11  HK fix bug in Sturms  TJP, put more into double precession
C 2009dec15  HK Add fourth file for single object. Initally Vesta
C 2012feb29  HK Minor Planets.  Handle either full JD or relative to 2440000 as inputs
C 2012nov21  HK Change element file names to all lower case and .tab extension
C               and change NAME to FILE in OPEN statement
C 2013feb02 HK Change base date from 1950 to J2000. All data files use C_END
C          Separate files for Solar system planets into orbits and spin vectors
C 2013jun19 HK Add flag for Pole position in B1950 ecliptic
C 2014jun09 HK Convert to R*8  Untabify    2016may23 HK remove unused variable
C_End&___1_________2_________3_________4_________5_________6_________.72

      INTEGER NFILES
      PARAMETER (NFILES=6)
      CHARACTER*12 FNAME(NFILES)
      DATA FNAME/'standish.tab','spinaxis.tab','minor.tab'
     &   ,'small.tab','comet.tab','exoplan.tab'/
C obsolete: 'seidel.tab','sturms.tab' 
      INTEGER*4 NLINES(NFILES)/2,1,13,12,10,11/ ! number of input lines per body
      INTEGER IGO               ! goto index
      INTEGER I,IERR,ISKIP,J,LIN, IYEAR,IMON,IDAY
      REAL*8 RDAY, P4,Q4
      REAL*8 OBL50 /0.40920619D0/ ! B1950 obliqity 23 deg 26' 44.836" in Radian
      REAL*8 EMJD               ! MJD of the elements returned
      REAL*8 EPOCH              ! Full Julian Day of elements
      REAL*8 DJBASE /2451545.D0/ ! J2000.0  All days offset from this.
      REAL*8 JULCEN /36525.D0/  ! Days in a Julian century; Standish06
      REAL*8 VVV(11),XX8        ! elements computed in loop 
      REAL*8 QP,XX
      REAL*8 ZBAXX(3)
      REAL*8 ALPHA,LAMBDA,BETA,SD,CACD,SACD,R,DELTA ! trig method
      REAL*8 FFF(6),RRR(6)      ! Items in file            
      REAL*8 TWOPI,PI,RDEG      ! numerical constants

      REAL*8 DANOM,DARGP,DCLIN,DECC,DODE,DPERIOD,DSJA,DPDEC,DPRA
      REAL*8 DLONP,DMLON,SIDYR

      REAL*8 JBRM(9) /0.99992568,0.011181483,0.0048590038 ! Rotation matrix
     2 ,-0.011181483, 0.99993748,-2.7170294e-05     ! from B1950
     3 ,-0.0048590038,-2.7162595e-05,0.99998819 /   ! to J2000

      CHARACTER*12 PNAME,TARGET ! object name
      CHARACTER*5 BUF5          ! hold word a beginning of line
      CHARACTER*25 MESS(7)      ! error messages
      DATA MESS /'opening table file' ,'EOF looking for C_END'
     & ,'error looking for C_END','EOF looking for target '
     & ,'error looking for target','EOF reading value'
     & ,'error reading value'/

C initialize constants
      TWOPI = 6.2831853071795864769D0 ! radians/revolution
      PI = TWOPI/2.
      RDEG = TWOPI/360.         ! radians/degree of arc
      SIDYR=365.256363004D0     ! siderial year in days 
      IERR=0                    ! set error index
      print *,'Entering PORBEL with IFILE,IPLAN=',IFILE,IPLAN
C If object is a planet, then need to process two files. Done by coming back
C to statement 40
      IGO=IFILE

C open data file and skip to desired object
 40   OPEN (UNIT=IOD, FILE=FNAME(IGO), STATUS='OLD',ERR=81)

C     skip past the header
 50   READ (IOD,'(a5)',END=82,ERR=83) BUF5 ! read one line of input file
      CALL UPCASE(BUF5,5)       ! change all to upper case
D       print *,BUF5
      IF (BUF5 .NE. 'C_END') GOTO 50 ! loop until  C_END found
      print *,BUF5    
      IF (IPLAN.GT.1) THEN      ! skip past objects
        ISKIP = NLINES(IGO)*(IPLAN-1)
        DO I=1,ISKIP            ! skip lines
          READ (IOD,*,END=84)   ! read a line without looking at it
        ENDDO
        print *,'skipped lines=',iskip
      ENDIF
      J=0                       ! count of successfull  READs
      GOTO (100,200,300,400,500,600),IGO ! type of file

C standish===========================================================
 100  EMJD=JULCEN*TCEN          ! MJD of elements returned
      READ (IOD,*,END=86,ERR=87) TITLE,FFF ! values at 2000.0
      J=1
D       print *,'title,fff= ',TITLE,FFF 
      READ (IOD,*,END=86,ERR=87) BUF5,RRR ! rates per century
      J=2
D       print *,'buf,rrr',buf5,rrr
C argument of perihelion: w = wbar-Omega= [5]-[6]
C mean anomaly: M= L-(Omega+wbar) =[4]-([6]+[5])
C check TJ0 before adding huge L rate
      DSJA =FFF(1)              ! a: semi-major axis, AU
      DANOM= FFF(4)-(FFF(5) )   ! mean anomaly at epoch <deg>
      DPERIOD=SIDYR*SQRT(DSJA**3) ! orbital period in days
      TJP = -DPERIOD*(DANOM/360.D0) ! date of perihelion
      print *,'base TJP=',TJP
      DO I=1,6                  ! move all values to request epoch
        FFF(I)=FFF(I)+TCEN*RRR(I) ! add the rate delta
      ENDDO
      DSJA =FFF(1)              ! a: semi-major axis, AU
      DECC =FFF(2)              ! e: eccentricity
      DCLIN=FFF(3)              ! i: inclination  <deg>
      DMLON=FFF(4)              ! L: Mean longitude <deg>
      DLONP=FFF(5)              ! wbar: longitude of perihelion <deg>
      DODE =FFF(6)              ! Omega: longitude of ascending node <deg>
C argument of perihelion is longitude of perihelion minus the
C longitude of the node
      DARGP=DLONP-DODE          ! argument of periapsis = wbar-Omega <deg>
C mean anomaly is mean longitude minus the longitude of perihelion
      DANOM= DMLON-DLONP        ! mean anomaly at epoch = L-wbar <deg>
      DANOM= MOD(DANOM,360.D0)  !+ value within 1 rev of zero
      DPERIOD=SIDYR*SQRT(DSJA**3) ! orbital period in days
      TJP = EMJD - DPERIOD*(DANOM/360.D0) ! date of perihelion
      CLOSE (UNIT=IOD)
      IGO=2
      GOTO 40                   ! read the spin vector file

C spin vector ===========================================================
 200  PNAME=TITLE
      WRITE(6,*) 'Orbit is ',TITLE,' ENTER:  / or satellite name'
      READ(5,*) PNAME
      LIN=LEN_TRIM(PNAME)       ! length of input name without trailing blanks
 220  READ (IOD,*,END=84,ERR=85) TARGET, RRR ! list directed
      IF (TARGET(1:LIN) .NE. PNAME(1:LIN) ) GOTO 220 ! loop until match
      I=LEN_TRIM(TITLE)         ! planet
      TITLE=TITLE(1:I)//':'//PNAME(1:LIN) ! Planet:pole
D       print *,'SP object=',target
D       print *,'RRR=',rrr
      DPRA =RRR(1)+TCEN*RRR(2)  ! pole ra
      DPDEC=RRR(3)+TCEN*RRR(4)  ! pole dec
      XX   =RRR(6)              ! rotation: degrees per day
      SIDAY = (360.d0*24.d0)/XX ! get length of siderial day in hours
      GOTO 800

C Minor planet ===========================================================
 300  READ (IOD,*,END=84)       ! BUF5    ! skip blank line
D       print *,'@300 ',BUF5
      J=1
      READ (IOD,*,END=86,ERR=87) TITLE ! get objects name
D       print *,'303 ',title
      J=2
D       print *,'TITLE=',title
      READ (IOD,*,END=86,ERR=87) EPOCH ! get epoch as Full Julian Date
      J=3
D       print *,'EPOCH=',epoch
      DO I=1,10                 ! each numeric entry
        READ (IOD,*,END=86,ERR=87) XX8 !  read one value
        J=3+I
        print *,I,XX8
        VVV(I)=XX8              ! 
      ENDDO 
      DSJA =VVV(1)              ! semi-major axis
      DECC =VVV(2)              ! eccentricity
      DCLIN=VVV(3)              ! inclination
      DODE =VVV(4)              ! longitude of node
      DARGP=VVV(5)              ! argument of perihelion
      DANOM=VVV(6)              ! Mean anomoly at epoch
      DPRA =VVV(7)              ! pole ra
      DPDEC=VVV(8)              ! pole dec
      QP   =VVV(9)              ! Prime meridian at epoch==Flag to B1950 ecliptic
      SIDAY=VVV(10)             ! siderial day length
      DPERIOD=SIDYR*SQRT(DSJA**3) ! orbital period in days
      TJP = (EPOCH-DJBASE) - DPERIOD*(DANOM/360.D0) ! date of perihelion
      IF (QP.EQ.1.) THEN        ! convert from B1950 ecliptic to J2000 equat.
C COCOMC uses west longitude, so negate RA
        CALL COCOMC(DPDEC,-DPRA,1.0, XX) ! unit vector along pole
        CALL ROTV(XX,1,OBL50,  P4) ! p4 now spinAxis in B1950 equator
        CALL ROTVEC (JBRM,P4, ZBAXX) ! rotate into J200 Equat.
        CALL COCOCM(ZBAXX, DPDEC,xx,q4) ! Q4 should be unity
        DPRA=-XX                ! from west to east longitude
        print *, 'DPRA,DPDEC,Q4=', DPRA,DPDEC,Q4
C using only trig
        lambda=rdeg*vvv(7)      ! RA in Radians
        beta=rdeg*vvv(8)        ! Dec in radians
C algoritym from Dmitry Savransky 
        sd = sin(obl50)*sin(lambda)*cos(beta) + cos(obl50)*sin(beta)
        cacd = cos(lambda)*cos(beta)
        sacd = cos(obl50)*sin(lambda)*cos(beta) -sin(obl50)*sin(beta)
        alpha = atan2(sacd,cacd)
        r = sqrt(cacd**2 + sacd**2)
        delta = atan2(sd,r)
        dpra= alpha/rdeg        ! Output RA in deg
        dpdec= delta/rdeg       ! output Dec in degrees
        print *, 'DPRA,DPDEC=', DPRA,DPDEC
      ENDIF
      GOTO 800

C Small bodies ===========================================================
 400  READ (IOD,*,END=86,ERR=87) I,TITLE,EPOCH
      J=1
      PRINT *,I,TITLE,EPOCH
      DO I=1,11                 ! each numeric entry
        READ (IOD,*,END=86,ERR=87) PNAME,XX8 !  read one value in 2nd item
        J=J+1
        PRINT *,I,PNAME,XX8
        VVV(I)=XX8              ! 
      ENDDO 
      DECC =VVV(1)              ! eccentricity
      DSJA =VVV(2)              ! semi-major axis
C do not use (3)=q
      DCLIN=VVV(4)              ! inclination
      DODE =VVV(5)              ! longitude of node
      DARGP=VVV(6)              ! argument of perihelion
      DANOM=VVV(7)              ! Mean anomoly at epoch
      SIDAY=VVV(8)/24.D0        ! siderial day length, days
      DPRA =VVV(9)              ! pole ra
      DPDEC=VVV(10)             ! pole dec
      QP   =VVV(11)             ! Prime meridian at epoch  NOT TRANSFERRED
      DPERIOD=SIDYR*SQRT(DSJA**3) ! orbital period in days
      TJP = (EPOCH-DJBASE) - DPERIOD*(DANOM/360.D0) ! date of perihelion
      GOTO 800

C Comets (spec. by q) ======================================================
 500  READ (IOD,*,END=84)       ! skip separation line
      J=1
      READ (IOD,*,END=86,ERR=87) TITLE ! get objects name
      J=2
D       print *,'TITLE=',title
      READ (IOD,*,END=86,ERR=87) IYEAR,IMON,RDAY ! get time of perihelion UTC
      J=3
D       print *,'year,mon,day=',IYEAR,IMON,RDAY 
      IDAY = RDAY               ! get integer days (UTC noon)
      CALL YMD2J2 (IYEAR,IMON,IDAY, I) ! get julian day base J2000.0
      TJP = I + (RDAY-FLOAT(IDAY)-0.5) ! add back the fraction of a day
      READ (IOD,*,END=86,ERR=87) DARGP ! get argument of perihelion
      J=J+1
      READ (IOD,*,END=86,ERR=87) DODE ! get longitude of node
      J=J+1
      READ (IOD,*,END=86,ERR=87) DCLIN ! get inclination
      J=J+1
      READ (IOD,*,END=86,ERR=87) QP ! get perihelion in au
      J=J+1
D       print *,'QP=',qp
      READ (IOD,*,END=86,ERR=87) DECC ! get eccentricity
      J=J+1
      DSJA = QP/(1.D0-DECC)     !  convert to semi-major axis
      DPERIOD=SIDYR*SQRT(DSJA**3) ! orbital period in days
      READ (IOD,*,END=86,ERR=87) DPRA,DPDEC ! get polar  RA and  Dec
      J=J+1
      READ (IOD,*,END=86,ERR=87) SIDAY ! get siderial day length
      J=J+1
D       print *,'SIDAY',siday   
      GOTO 800

C Exo-planets ======================================================
 600  READ (IOD,*,END=84) BUF5  ! skip separation line
      print *,'600 buf=',BUF5
      J=1
      READ (IOD,*,END=86,ERR=87) TITLE ! get objects name
      PRINT *,TITLE
      J=2
      DO I=1,9                  ! each numeric entry
        READ (IOD,*,END=86,ERR=87) XX8 !  read one value in 2nd item
        J=J+1
        PRINT *,I,XX8
        VVV(I)=XX8              ! 
      ENDDO 
      XX8=10.**(-0.4*(VVV(1)-4.83)) *(VVV(2)/32.616)**2 ! Star/Sun power factor
      Print *,'Star/Sun power factor=',xx8,' * default SOLCON M <><>'
      TJP=VVV(3)-DJBASE         ! MJD date of periastron
      DSJA =VVV(4)              ! semi-major axis in AU
      DPERIOD=VVV(5)            ! orbital period in days
      DECC =VVV(6)              ! eccentricity
      XX8=MAX(VVV(7),0.1)       ! obliquity. Must avoid 0. which causes NANs
      DPDEC=90.-XX8             ! pole Dec, based on obliquity in S system
      DARGP=VVV(8)              ! argument of perihelion == Ls of periap
      SIDAY=VVV(9)/24.          ! siderial day length, days
      XX8=SIDAY*(1.+ SIDAY/DPERIOD) ! Synodic (solar) day
      Print *,'Synodic day =',XX8,'  to KRC PERIOD <><>'
C Items defined by use of the Seasonal coordinate system
      DCLIN=0.                  ! inclination
      DODE =0.                  ! longitude of node
      DANOM=0.                  ! Mean anomoly at epoch
      DPRA =270.                ! pole RA
      GOTO 800
C =================================================================
 800  RODE = DODE*RDEG          ! | convert output arguments 
      CLIN = DCLIN*RDEG         ! | to double precision radians
      ARGP = DARGP*RDEG         ! "
      XECC = DECC
      SJA = DSJA
      PRA = DPRA*RDEG           ! "
      PDEC = DPDEC*RDEG         ! "
      PERIOD= DPERIOD           ! orbital period in days
      IF (IOP.NE.0) THEN
        WRITE(IOP,801) IFILE,IPLAN,FNAME(IFILE),TITLE, TCEN
     &  ,DODE, DCLIN,DARGP,  RODE,CLIN,ARGP
     &  ,DSJA,DECC,TJP,PERIOD,  SIDAY, DPDEC,DPRA, PDEC,PRA
 801    FORMAT ('0IFILE IPLAN FILE TITLE TCEN ='/2I4,2X,A,2X,A10,F10.6
     &/'[double] ODE CLIN ARGP /radian =' /3F15.8 /3F15.8 
     &/' SJA ECC TJP PERIOD ='/2F15.8,f15.5,F15.8
     &/' SIDAY   [double] PDEC PRA  /radian ='/F15.8/2F15.8/2F15.8)
      ENDIF

 9    CLOSE (UNIT=IOD)
D       print *,'Exit PORBEL'
      RETURN
C_____________________________________________________________________

C  Error section
 87   IERR=IERR+1               ! error reading values
 86   IERR=IERR+1               !  EOF reading values
 85   IERR=IERR+1               ! error skipping objects
 84   IERR=IERR+1               !  EOF skipping objects
 83   IERR=IERR+1               ! error looking for  C_END
 82   IERR=IERR+1               !  EOF looking for  C_END
 81   IERR=IERR+1               ! opening file

      PRINT *,'PORBEL Error: File was >',FNAME(IGO)
      PRINT *,'------ Error: ierr',IERR,' Last title= ', TITLE
      PRINT *,'------ Error: J=',J,' Reason: ',MESS(IERR) 
      GOTO 9
      
      END
