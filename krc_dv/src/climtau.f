      REAL*4 FUNCTION CLIMTAU (LSUB,ALAT,TAUICE)
C_Titl  CLIMTAU  Atmospheric opacities as function of Ls and latitude
      IMPLICIT  NONE
      INTEGER*4 NUMX,NUMY
      PARAMETER (NUMX=72)      ! number of "sample"
      PARAMETER (NUMY=36)      ! number of "lines"
C_Vars
      INCLUDE 'unic8m.f'
      INCLUDE 'filc8m.f'
C_Args
      REAL*4 LSUB               !in. season; l-sub-s in degrees.
C     If large negative, will read file and save values
      REAL*4 ALAT               !in. Latitude in degrees
      REAL*4 TAUICE             !out. opacity of water-ice cloud
C     Function                  !out. opacity of aerosol dust
C_Desc
C  Initally reads a file of opacity versus Ls and latitude
C  For each positive date request, does linear interpolation, with wrap around.
C output will be negative if error occured
C File dimensions firm-coded here
C_Hist  Hugh_Kieffer  2012mar20 Derive from seastau.f
C 2012mar27 HK  Include tau-ice 
C 2016may12 HK Update include names
C_End6789012345678901234567890123456789012345678901234567890123456789012_4567890

C local variables
      REAL*4 DDATA(NUMX,NUMY,2) ! holds the table
      INTEGER*4 MAXH /512/      !! These 2
      CHARACTER*512 HEADER      !! must agree
      INTEGER*4 ID(10)            ! array type and dimensions
      INTEGER*4 I,J             ! number of defined dates
      INTEGER*4 RET             !  BINF5 return code  
      REAL*4 DELLS,DELLAT,OUT
      REAL*4 FX,FY              ! interpolation parameters
      REAL*4 Y1,Y2              ! intermediate interpolation
      SAVE DDATA,DELLS,DELLAT

      IF (IDB1.GE.5) WRITE(*,*)'CLIMTAU',LSUB ! 
      OUT=-1.                   ! possible error flag

      IF (LSUB .LT. -90.)  THEN ! read the file
         DELLS=360./FLOAT(NUMX)
         DELLAT=180./FLOAT(NUMY)
         ID(1)=3
         ID(2)=NUMX             ! num seasons
         ID(3)=NUMY             ! num latitudes
         ID(4)=2                ! tau dust and ice
         ID(8)=4                ! singPrec float
         ID(9)=MAXH             ! inform routine how much room for header
         RET=3                  ! ask for both debug
         CALL BINF5 ('R',FVTAU, HEADER,ID,DDATA, RET)
         PRINT *,'CLIMTAU RET=',RET
         PRINT *,'ID=',ID
         PRINT *,HEADER
C         Print *,(DDATA(I,I,0),I=1,NUMY)
        IF (RET.LT. 0)
     +          WRITE(IOERR,*)'CLIMTAU error opening input file>',FVTAU
        OUT=RET

        ELSE                    ! bi-linear interpolate

           FX=AMOD(LSUB,360.)   ! ensure in range
           FX=FX/DELLS +1.
           FX= MIN(MAX(1.0,FX),REAL(NUMX)-1.0001)
           I=INT(FX)            ! index of low point
           FX=FX-I              ! fraction of upper point
           FY=(ALAT+90.)/DELLAT +0.5
           FY= MIN(MAX(1.0,FY),REAL(NUMY)-1.0001)
           J=INT(FY)            ! index of low point
           FY=FY-J              ! fraction of upper point
           Y1= (1.-FX)*DDATA(I,J,1) +FX*DDATA(I+1,J,1) ! 1)=dust
           Y2= (1.-FX)*DDATA(I,J+1,1) +FX*DDATA(I+1,J+1,1)
           OUT=(1.-FY)*Y1+FY*Y2 ! tau dust
           Y1= (1.-FX)*DDATA(I,J,2) +FX*DDATA(I+1,J,2) ! 2)=ice
           Y2= (1.-FX)*DDATA(I,J+1,2) +FX*DDATA(I+1,J+1,2)
           TAUICE=(1.-FY)*Y1+FY*Y2 ! tau ice

      ENDIF

      CLIMTAU=OUT
      IF (IDB1.GE.5) WRITE (*,*) 'CLIMTAU out', LSUB,ALAT,OUT,TAUICE
      RETURN
      END
