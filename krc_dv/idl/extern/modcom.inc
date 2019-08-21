C_Titl  modcom.inc  include common for  TES thermal inertia code system
C_Vars
      INTEGER*4  NHOUR,NLAT,NPRES,NTAU,NUMI ! dimensions
      INTEGER*4  NUMA,NIA,NIA2,NMOD,NMOD2,NHP1,NHP3   ! dimensions
      PARAMETER (NHOUR=24)      ! number of hours in model
      PARAMETER (NLAT=37)       ! number of latitudes in model
      PARAMETER (NTAU=3)  	! number of opacities in model
      PARAMETER (NPRES=3) 	! number of pressures in model
      PARAMETER (NUMI=10)       ! number of inertias in model set at base albedo
      PARAMETER (NUMA=3)        ! number of ancillary albedo models
      PARAMETER (NIA=NUMI*NUMA)	! total number of inertia and albedo models
      PARAMETER (NMOD=NPRES*NTAU*NIA) ! total number of all models
      PARAMETER (NMOD2=2*NMOD)  ! total number of all models &  T sets
      PARAMETER (NIA2=2*NIA)    ! total number of  A/I values, 2 sets
      PARAMETER (NHP1=NHOUR+1)   ! number to keep in  _HLM arrays
      PARAMETER (NHP3=NHOUR+3)   ! number used for spline fits

      REAL*4 HOUR1,HOURDEL,ALAT1,ALATDEL ! model grid
      REAL*4 ALBDEF			! default albedo (for use at night)
      REAL*4 VT(NTAU)			! values of opacity in models
      REAL*4 VP(NPRES)			! values of pressure in models
      REAL*4 VI(NUMI)			! values of inertia in models
      REAL*4 VA(NUMA)			! values of albedo in models
      REAL*4 THLM(NHP1,NLAT,NMOD2)	! model interpolated to date
      REAL*4 DHLM(NHP1,NLAT,NMOD2)	! spline derivatives
      REAL*4 HOURS(NHP1)		! set of hours used for splines
      REAL*4 PMEAN,SCALEHM              !  Mars mean pressure and scale-height
      REAL*4 TPOLE(2,NMOD2)             ! South and North Pole temperatures
      COMMON/MODELS/ HOUR1,HOURDEL,ALAT1,ALATDEL,
     &   VT,VP,ALBDEF,VI,VA, PMEAN,SCALEHM, HOURS, THLM,DHLM,TPOLE
C Each model is (hour,latitude)
C Model order in  THLM &  DHLM is:  [(I&A,tau,PRES),surf:top)]
C Where there is a full set of surface kinetic temperature models followed by 
C   a full set of Planetary brightness temperature models.
C 
C  Option for not having opacity or pressure as variables;
C set  NTAU=1 &  NPRES=1
C and fix  modinit.f to have only one value for  VP &  VT
C  and then recompile and relink
C_Hist 97aug12  Hugh Kieffer  USGS_Flagstaff
C  97sep09  HK Accomodate Planetary brightness temperatures
C  98apr24  HK Convert to bi-cubic splines for hour/latitude
C  98may06  HK Include  Mars mean values
C  98may28  HK Set to large model set
C 2008jan30 HK Celestial Reasonings: Include TPOLE
C..............

