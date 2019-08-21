      REAL FUNCTION ELEVATE (DLAT,DLON)
C_Titl  ELEVATE elevations from  Mars  DTM
      IMPLICIT NONE
C_Vars
        INCLUDE 'errcom.inc'
C_Args
      REAL*4 DLAT	!in. latitude, degrees north
      REAL*4 DLON	!in. longitude, degrees west
C_Desc
C  Reads file based on  Mars  DTM  CD-ROM of 1/64 degree resolution
C   file must be simple cylindrical
C   current code is for 1/4 degree resolution; 
C   to change, modify  DEM file name and  IDEG
C accepts longitudes +/- 540.
C  Has full error checking on latitude,
C   code could be faster if tests were eliminated.
C_File 
C  DEM file must have raster array starting at beginning of  IPOINT block 
C of 512 bytes.  Array must have first line at North, 
C and first sample at 180 degree west.
C  Uses primio system to open, read, close file. 
C   does not conflict with  FORTRAN units
C_Lims
      integer ideg,nl,ns
      PARAMETER (IDEG=4) ! pixels per degree
C definition of  DEM file
      CHARACTER*60 FILENAME / '/hob1/hkieffer/tes/dtm4.cub'/ !  DEM file
      INTEGER IPOINT /48/       ! offset to start of core object in  ISIS cube
C_Hist 97mar11 aug02  Hugh_Kieffer original version
C_End6789012345678901234567890123456789012345678901234567890123456789012_4567890

      PARAMETER (NL=IDEG*180 , NS=IDEG*360) ! number of lines and samples
      INTEGER*2 DEM(NS,NL)
      INTEGER KYLO /11/    ! assign error counter for invalid low  latitude
      INTEGER KYHI /12/    ! assign error counter for invalid high latitude
      INTEGER KXLO /13/    ! assign error counter for invalid low  longitude
      INTEGER KXHI /14/    ! assign error counter for invalid high longitude
      LOGICAL LFIRST /.TRUE./ ! has not been initialized

      REAL*4 BASE /-6./         ! transform from integer to elevation in km.
      REAL*4 SCALE / 2.E-3/     ! "  "  "

      integer nbh,nd1,nbd, i1,i2,j1,j2,FID,nbtot,ierr
      real*4 el,el1,el2
      real*4 pix,xx0,yy0,xxdel,yydel,bx,x,xr,y,yr

        IRET=0 ! set return code to normal

      IF (LFIRST) THEN          ! if first time, initialize
        NBH=512*(IPOINT-1)       ! # bytes in label
        ND1=NBH+1               ! first byte to read
        NBD=NS*NL*2             ! number of bytes in array to read

        CALL PIO_IN (FID, FILENAME, NBTOT, 0, IERR) !  OPEN existing file
        IF (IERR .NE. 0) GOTO 81
        CALL PIO_RD (FID, ND1,NBD, DEM, IERR) ! read the  DEM array
        IF (IERR .NE. 0) GOTO 82
        CALL PIO_CL (FID,0,IERR) !  CLOSE; do not delete (error unlikely)

        PIX = 1./FLOAT(IDEG)    ! size of one pixel
        XX0=180.-PIX/2.         ! base for x=sample=-longitude system
        YY0=90.-PIX/2.          ! base for y=line=-latitude system
        XXDEL=-PIX              ! increment for x
        YYDEL=-PIX
        IF (IDB3.GT.0) WRITE(IDB3,*)'xx/yy 0&del',XX0,YY0,XXDEL,YYDEL
        LFIRST=.FALSE.
      ENDIF

C determine location of target geographic point within  DEM array
      BX=DLON                   ! longitude
      IF (BX.GT.180.) THEN      ! adjust longitude into -180:180
        BX=BX-360.
      ELSEIF (BX.LT.-180.) THEN
        BX=BX+360.
      ENDIF
      IF (BX.GE.-180.) THEN     ! CAN'T USE INTEGER TESTS BECAUSE -.9 ROUNDS TO 0
        X=(BX-XX0)/XXDEL        ! FIND INDEX AND FRACTION FOR  DLON
        I1=INT(X)
        IF (I1.LT.1) THEN       ! DLON  MUST FALL IN interval below first point
          XR=X                  ! SET TO INTERPOLATE IN WRAP-AROUND
          I1=Ns
          I2=1
        ELSEIF (I1.GE.Ns) THEN  !  DLON WAS TOO HIGH
          KERR(KXHI)=KERR(KXHI)+1 ! INCREMENT ERROR COUNT
          Xr=1.                 ! EFFECTIVELY SET  DLON TO UPPER VALID LIMIT
          i1=ns-1
          i2=ns
        ELSE                    !  DLON IS INTERIOR TO MODEL POINTS
          I2=I1+1
          XR=X-REAL(I1)
        ENDIF
      ELSE                      !  DLON WAS TOO SMALL
        KERR(KXLO)=KERR(KXLO)+1 ! INCREMENT ERROR COUNT
        X=0.0                   ! EFFECTIVELY SET  DLON TO LOWER VALID LIMIT
        I1=ns
        I2=1
      ENDIF

      Y=(DLAT-YY0)/YYDEL        ! find index and fraction for latitude
      J1=INT(Y)
      IF(J1.LT.1) THEN          ! latitude was too small
        KERR(KYLO)=KERR(KYLO)+1 ! increment error count
        Y=1.0                   ! effectively set  Y to lower valid limit
        J1=1
      ENDIF
      J2=J1+1
      IF(J2.GT.NL) THEN       ! latitude was too high
        KERR(KYHI)=KERR(KYHI)+1 ! increment error count
        Y=REAL(NL)-.01        ! effectively set  Y to upper valid limit
        J2=NL
        J1=J2-1
      ENDIF
      YR=Y-REAL(J1)


C bi-linear interpolation of  DEM array
        EL1 = (1.-XR)*FLOAT(DEM(I1,J1))+XR*FLOAT(DEM(I2,J1))
        EL2 = (1.-XR)*FLOAT(DEM(I1,J2))+XR*FLOAT(DEM(I2,J2))
        EL = (1.-YR)*EL1 + YR*EL2
        ELEVATE = BASE+SCALE*EL   !scale to kilometers

      IF(IDB3.NE.0)WRITE (IDB3,333)DLAT,DLON,I1,I2,J1,J2,XR,YR,ELEVATE
 333  FORMAT('lat,lon,i12,j12,xr,yr,el=',2F7.2,4I5,2f5.2,F7.3)

 9    RETURN

C error section
 81   WRITE (IOERR,*)'ELEVATE: open failure for',FILENAME
      IRET=-1
      GOTO 9
 82   WRITE(IOERR,*)'ELEVATE: Read error in PIO',IERR
      IRET=-2
      GOTO 9
      END
