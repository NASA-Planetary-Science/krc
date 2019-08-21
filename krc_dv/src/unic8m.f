C_Titl  unic8m.f   common  /UNITS/ for logicIO units and errors  KRC
      INTEGER IOPM    ! logical unit for prompt, usually terminal screen
     &,IOKEY    ! logical unit for interactive input, usually terminal keyboard
     &,IOIN     ! logical unit for input file
     &,IOSP     ! logical unit for printer (spooled)
     &,IOERR    ! logical unit for error messages, commonly =  IOSP
     &,IOD1     ! disk unit for explanation file & zone table (briefly open)
     &,IOD2     ! direct-access datafiles
     &,IOD3     ! direct-access far-field temperatures
     &,IRTN     ! subroutine # in which error occured
     &,IERR     ! error return code or internal error code.
     &,MINT     ! the number of temperature sets in file being handled by IOD3  
      INTEGER IDB1,IDB2,IDB3,IDB4,IDB5,IDB6   ! debug control
      LOGICAL LOPN1 ! spare    | status of logical IO units
     &,LOPN2,LOPN3    ! IOD2,3   | .TRUE. means that one is currently open
     &,LOPN4          ! Type 5x  |,LOPN2,LOPN3,LOPN4
      COMMON /UNITS/ IOPM,IOKEY,IOIN,IOSP,IOERR,IOD1,IOD2,IOD3,IRTN,IERR
     &,MINT, IDB1,IDB2,IDB3,IDB4,IDB5,IDB6, LOPN1,LOPN2,LOPN3,LOPN4
C_Desc
C IDB tests all write to IOSP
C IDB1 1=mseas_enter 5=tcard_enter
C IDB2 1=tlats_enter 5=tday_enter  6=tday_exit
C IDB3 1=tdisk_enter 3=tdisk_exit
C IDB4 n=TDAY  
C_Hist  1985----  Hugh_H_Kieffer
C 2004jul06  HK  Explicit type statements
C 2010jan12  HK  Remove '*4' from type statements
C 2010apr21  HK  Add debug control
C 2016feb12  HK  Only changes to comments
C 2016may13  HK  Add MINT 
C_End____________________________________________________________________________
