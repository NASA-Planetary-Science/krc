      SUBROUTINE PORBIG (TITLE)
C_Titl  PROBIG  Read orbital elements via PORBEL. Initiate porbcm
      INCLUDE 'porbc8m.f'     ! has  IMPLICIT NONE
C_Args 
      CHARACTER*(*) TITLE       !out. object name
C_Desc
C  Computes orbital elements and rotation matrices of date into common
C Responds to  PBUG  in  PORBCM: bit-encoded
C   +1 Show orbit definition, AFRM and spinaxis in degrees
C   +2 Show input spin axis in radians, 4 vectors and BARM
C   +4 Show BFRM
C_Calls:  PORBEL  COCOSC  ROTCOL  ROTAX  ROTORB  ROTROW  ROTSHO  ROTEST
C         ROTVEC  TRANS3  VCROSS  VDOT  VNORM  VSHOW
C_HIST  2013jul16 Hugh Kieffer Modified substantially from  PORB1
C        Add bit-encoded debug. All lines marked with !d
C 2013aug24 HK Add JPL small bodies table and exoPlanets table
C 2014jun09 HK Untabify, Convert to R*8         Link with rotmdp8.f etc
C_End&___1_________2_________3_________4_________5_________6_________.72

      INTEGER I,J,K,INCODE,IPLAN
      REAL*8 PIO2               ! constants
      REAL*8 PERIOD
      REAL*8 QQ,QB,QV(3)        ! temporary scalar and vector   
      REAL*8 XBFXU(3),YBFXU(3),ZBFXU(3),ZBAXU(3),ZFAXU(3)     
      REAL*8 ZUNIT(3) /0.,0.,1./ ! Z unit vector
      REAL*8 AFRM(9),FARM(9)
C
      PIO2=PICON/2.D0           ! pi over 2
C
 1    WRITE (IOS,*) ' ?* SOURCE DATA: -=quit '
     & ,'1=Planets 3=minor 4=small [5=comets] 6=exoPlanets '
      READ (IOK,*,END=9,ERR=1) INCODE
      IF (INCODE.LT.1) RETURN

 11   WRITE (IOS,*) ' ?* WHICH item in file.'
      READ (IOK,*, END=9,ERR=11) IPLAN
      IF (INCODE.EQ.1) THEN
 2      WRITE (IOS,3)
 3      FORMAT (' ?* EPOCH desired as centuries after 2000.0')
        READ (IOK,*,ERR=2,END=9)TC
      ENDIF

      K=0
      WRITE (IOS,*) ' ?* Debug print: 0:7 bit encoded See porbig.f'
      READ (IOK,*,END=9,ERR=1) K
      PBUG=REAL(K)              ! transfer into common

      CALL PORBEL (IOP,IOD,INCODE,IPLAN,TC, RODE,CLIN,ARGP
     &,XECC,PERIOD,SJA,SIDAY,ZBAB,ZBAA,TJP,TITLE) ! ZBAB,ZBAA = RA,Dec
      OPERIOD=PERIOD            ! transfer to common
      WRITE (IOS,*)TITLE
      WRITE (IOP,*)' INPUT FILE WAS: ',TITLE 

      EOBL= .409092601D0        ! ecliptic obliquity 84381.406" in radians IAU2009
C----------------------------Calculations-----------------------------
      PLANUM=100.*INCODE+IPLAN  ! load object number into common

C Get rotation matrix from orbital==F to J2000==A rotation matrix  AFrm
C elements are relative to Earths orbit, not Earth Equator
      CALL ROTORB (RODE,CLIN,ARGP, AFRM,.TRUE.) ! This is EFrm
      IF (AND(K,1).NE.0) THEN          !d 1's bit was set
        WRITE(IOP,*),'IPLAN,TC,SIDAY,OBL=',IPLAN,TC,SIDAY,EOBL
        WRITE(IOP,*),'ODE,CLIN,ARGP=',RODE,CLIN,ARGP !d
        WRITE(IOP,*),'ECC,PERIOD,SJA=',XECC,PERIOD,SJA
        WRITE(IOP,*),'ZBAB,ZBAA,TJP=',ZBAB,ZBAA,TJP ! RA and Dec in radians
        CALL ROTSHO ('EFRM',AFRM,IOP) !d
      ENDIF 

C for solar system objects, Add rotation from ecliptic to equatorial  
 ! ExoPlan works in S system 
CX      print *,'INcode=',incode,eobl
CX      print *,'AFRM',afrm
      IF (INCODE.NE.6) CALL ROTAX  (1,-EOBL,  AFRM) ! by obliquity to J2000
CX      print *,'INcode=',incode,eobl
CX      print *,'AFRM',afrm
      CALL ROTCOL (AFRM,3, ZFAXU) ! Orbit pole , Z unit vector, in J200
      IF (AND(K,2).NE.0) THEN          !d 2's bit was set
        CALL ROTSHO ('AFRM',AFRM,IOP) !d
        QB=R2D*ZBAB             !d RA
        QQ=R2D*ZBAA             !d dec
        WRITE(IOP,*)'Spinaxis RA and Dec, degrees:',QB,QQ !d
      ENDIF                     !d
C Get rotation matrix from  J2000==A to  Seasonal==B
C inputs: ZBAB = Right Ascension of spin axis in J2000 equitorial, degrees
C         ZBAA = Declination " "         
C Z axis of B is along spin axis
C X axis of B is along vernal equinox, which is along spinAxis-cross-OrbitPole
      CALL COCOSC (PIO2-ZBAA,ZBAB, 1.d0 , ZBAXU) ! spin axis unit vector in J2000
      CALL VDOT (ZFAXU,ZBAXU, QQ) ! cos of angle between orbit Z and body Z
      BLIP=ACOS(QQ)             ! angle between vectors, == obliquity
      CALL TRANS3 (AFRM,FARM)
      CALL ROTVEC (FARM,ZBAXU, ZBFXU) ! rotate into Focus system

C Vernal equinox, which is along spinAxis-cross-OrbitPole
C    but orbit pole in F is [0,0,1] 
      CALL VCROSS (ZBFXU, ZUNIT, QV) ! QV is XBFXX
      CALL VNORM (QV,XBFXU)     ! make unit length:  Vernal equinox
      CALL VCROSS (ZBFXU,XBFXU,YBFXU) ! get Y axis of Season system
C Make rotation matrix direct from orbit plane to Season
      DO I=1,3                  ! each element of the axis vectors
        J=3*I
        BFRM(J-2)= XBFXU(I)     ! transfer X_new==B in old==A
        BFRM(J-1)= YBFXU(I)     ! transfer Y " " 
        BFRM(J  )= ZBFXU(I)     ! transfer Z " " 
      ENDDO
      TAV=ATAN2(XBFXU(2),XBFXU(1)) ! True anomaly at VE
C-      I=ISHFT(K,-2)             !d right shift 2 places to 4's bit
C-      I=MOD(I,2)                !d this is +4 
C-      IF (I.EQ.1) then          !d
      IF (AND(K,4).NE.0) THEN          !d 4's bit was set
        CALL VSHOW ('ZBAXU',ZBAXU,-IOP) !d  Print with col headings
        CALL VSHOW ('ZBFXU',ZBFXU,IOP) !d
        CALL VSHOW ('XBFXU',XBFXU,IOP) !d
        CALL VSHOW ('YBFXU',YBFXU,IOP) !d
        WRITE (IOP,*)'TAV: rad,deg',TAV,R2D*TAV
        CALL ROTSHO ('BFRM',BFRM,IOP) !d
        CALL ROTEST (BFRM, QB,QQ) !d
        WRITE (IOP,*)'ROTEST der and dev=',QB,QQ !d
      ENDIF
      
 9    RETURN
      END
