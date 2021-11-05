      SUBROUTINE TFINE8 (IQ, KTT,DENN,CTT,IKK, TRET,IRET)
C_Titl  TFINE  KRC higher resolution layer computations
C_Vars  
CD      IMPLICIT  NONE
      INCLUDE 'krcc8m.f'        ! has  IMPLICIT  NONE.  Need,  J4,J5...  many
      INCLUDE 'dayc8m.f'    ! need  FINSOL, TTJ, the initial temperature profile
      INCLUDE 'hatc8m.f'        ! need  PARC,CCC
      INCLUDE 'unic8m.f'        ! need  IOD1
      INCLUDE 'filc8m.f'        ! need  VERSIN,FRUN
C      INCLUDE 'porbc8m.f'       ! need  SJA
C_Args
      INTEGER*4 IQ      ! in. 1=initialize    2=day computations    3=write bin5
      REAL*8 KTT(MAXN1P)        ! in.  Thermal conductivity of each layer
      REAL*8 DENN(MAXN1P)       ! in.  Density of each layer 
      REAL*8 CTT(MAXN1P)        ! in.  Specific heat of each layer
      INTEGER*4 IKK(4)          ! in.  layer indices for  kofT
      REAL*8 TRET(MAXN1P)       ! out.  Temperature profile on original layers
      INTEGER*4 IRET            ! out.  Deepest normal layer treated
!  Negative is an error
!  TFINE(1,  -8=too many fine-times  
!  TFINE(2,  -7=ECLIPSE error return   -2=numerical blowup 
C   
C_Desc Makes set of layers finer by a factor of F and time divisions finer by F^2
C Interpolates initial depth profile to the new layers
C Calls  ECLIPSE to get upper boundary conditions to the new times 
C Runs forward for the specified time span once and returns the depth profile
C for the input set of layers.
C Accounts for far-field heating, see variable ZFAR below
C Coded for only bare surface; no atmosphere or frosts
C This routine independent of zone / no zone.
C Because max  N2 can exceed  I*2 range, make all integers transferred  I*4
C_Calls   EVMONO3D  E  RNDEX   TUN8
C version 3.5 Does not allow daily eclipse and atmosphere at once
C_Lien
C No "SAVE" statement here; assumes that all the values from IQ=1 are held IQ=2 
C_Hist 2017mar13 Hugh_Kieffer   Derive from tday8.f
C 2020apr12 Correct KJ from R*8 to I*4
C_End6789012345678901234567890123456789012345678901234567890123456789012_4567890

C for fine layers
      INTEGER*4 MAXCK,MAXFK,MAXFL,MAXFLP,MAXFAC,MAXCCC,MAXJ79
      PARAMETER (MAXCK=4)       ! Number of coarse layers to store as fine
      PARAMETER (MAXFK=20)      ! maximum number of items to store each fine-time
      PARAMETER (MAXFL=99)      ! maximum number of fine layers
      PARAMETER (MAXFLP=MAXFL+1) ! dimension layer temperature points
      PARAMETER (MAXFAC=49)      ! maximum time factor
      PARAMETER (MAXJ79=3+3*MAXN4) ! maximum for  J4,J7P and  J9 storage
      PARAMETER (MAXCCC=5000000) ! maximum storage for output file

      REAL*8 KTF(MAXFLP)        ! thermal conductivity of fine layers 
      REAL*8 CTF(MAXFLP)        ! specific heat of fine layers
      REAL*8 DENF(MAXFLP)       ! density of fine layers
      REAL*8 TTF(MAXFLP)        ! temperature of fine layers
     &,SCONVF(MAXFL)        ! Classical convergence factor for each fine layer
     &,BLAF(MAXFLP)         ! fine Layer thicknesses [m]
     &,XCEF(MAXFLP)         ! fine Layer depth
      REAL*8 CCC(MAXCCC)    ! bin5 array
C
      REAL*8 FA1(MAXFL), FA2(MAXFL), FA3(MAXFL),DTJ(MAXFL) ! each max # layers
      REAL*8 DIFFI(MAXFLP)
      INTEGER*4 KJ(MAXN2)   ! bottom layer for calculations at each time step

      INTEGER*4 MM3,JJJ(10),J79(MAXJ79)       ! sizes to go to  BINF5
      INTEGER*4 I,II,IH,J,J1,J9,JLOW
     &,K,KC,KFL,KFT,KKF,KN,KM,N1F,N1FP,N2F,NECL
      INTEGER*4 J7P        ! index of ctime interval containing eclipse start
      INTEGER*4 J8         ! index of ctime interval containing eclipse end
      INTEGER*4 JJ         ! global (all day) coarse time index
      INTEGER*4 JFI        ! global (all eclipse) fine time index
      INTEGER*4 JL         ! local fine time relative to last radiation update
      INTEGER*4 JLS        ! local fine time at which to increment coarse radiation
      INTEGER*4 JGU        ! global fine time when coarse radiation updated
      INTEGER*4 NEEP       ! number of items to store each fine time step
C
C     it uses  DOWNIR for Planet thermal load and  
      INTEGER*4 JBE(4),PARI(2)    ! time-step range indices and input arguments
      INTEGER*4 N1Z(MAXBOT)     ! Binary time division layers
      REAL*8 ABRAD,BF1,DELBOT,DELT
     &,DTIM,DTIMI,EMTIR,FAC3,FAC3S,FAC4,FAC45,FAC5,FAC6
     &,FAC6F,FAC7,FAC8
     &,PERSEC,POWER,SHEATF
     &,TSUR,TS3,TSUR4,FCJ
      REAL*8 QA,QB,QQ,RLAF           ! temporary use
      REAL*8 FALB0,FALBD,FSOL0,FSOLD,FFAR0,FFARD,FPLA0,FPLAD ! time interpolation
     & ,FDIF0,FDIFD, FDIF,ZFAR,FPLA,FSOL ! time interpolation
      REAL*8 TIFAC(MAXFAC),YTF(MAXN1P),YTR(MAXFLP)
      REAL*8 ZDZ(MAXFLP)
      REAL*8 FINSJ ! holds  FINSOL during eclipse and 1.0 during follow-on
      CHARACTER*2 BUFF
      CHARACTER*80 FUFF         ! ----tfinenn.bin5
      CHARACTER*280 BONG        ! buffer of long constructions 20*13 +4
      CHARACTER*1100 HEADTX     ! bin5 header 38*3*6=684 +12*13+4=160 +bong
C variables for k(T)
      REAL*8 FBI(MAXFL),FCI(MAXFL) ! layer factors
      REAL*8 FBK,FBKL,FA1J,FA3J ! temporary factors
      INTEGER*4 IK1,IK2,IK3,IK4   ! layer indices for  kofT
C      LOGICAL LATM              ! there is an atmosphere
      LOGICAL LATOK             ! there is room in  CCC to store current latitude 
      LOGICAL LBASE             ! maintain basal heat flow at eclipse start
      LOGICAL LPH               ! consider planetary heat load
      LOGICAL LFIN              ! True = within the range of  FINSOL calculations
      LOGICAL LALCON            ! all layers  T-constant 

C Variables for far flat: indicated by  LOPN3 true
      LOGICAL LSELF ! Not using far-field temperatures
C      Logical LODD ! layer expansion factor is odd ( vrs even)
C
      IF (IDB5.GE.1) WRITE(IOSP,*) 'TFINE IQ,J4,J5=',IQ,J4,J5
      SELECT CASE (IQ)

      CASE(1) !======================== initialize for all lats ==================
C
C  Must be called here once per case.
C  Set up grid based upon nominal conductivity, then use  T-dependant values
C in the time loops.  Assumption here is that conductivity could depend on
C several variables, but that having all but  T constant for a given case is 
C adequate.
C  The convergence safety factor should be chosen to be adequate to cover 
C the conductivity variation.

      LPH =(PARW(1).GT. 0.)     ! doing planetary heat loads
      QA=PARC(1)                ! layer factor. 
      KFL=NINT(QA)              ! integral fine layer factor. 
      KFT=KFL*KFL               ! fine time factor
      IF (KFT .GT.MAXFAC) THEN
        WRITE(IOERR,*)'TFINE: Maximum time factor exceeded:=',KFT
        IRET=-7
        GOTO 9
      ENDIF
      QB=DBLE(KFL)              ! layer factor as real
      QQ=LOG(RLAY)/QB           ! new geometric ratio. eq. rk 
      RLAF=EXP(QQ)              ! in two steps  
      BF1=(RLAF-1.)/(RLAY-1.)   ! factor for first sub-layer. eq. f1
C for fine layers, xTT becomes xTF
      N1F=1+(N1-1)*KFL ! Number of fine layers, including virtual but not base
      N1F = MIN0(N1F,MAXFL)     ! number of fine layers
      N1FP=N1F+1                ! including the base
      JLOW= (N1F-1)/KFL +1      ! Lowest  TDAY layer matched 
      IRET=JLOW
      IF (IDB5.GE.1) WRITE(IOPM,*)'QB..',QB,RLAY,RLAF,BF1,N1F,JLOW
      PERSEC = PERIOD * 86400.  ! get solar period in seconds
      N2F=N2*KFT                ! number of fine timesteps per sol
      DTIM=PERSEC/DBLE(N2F)     ! size of one fine time step, sec
      LBASE = (PARC(8) .GE. 0.) ! set base treatment

C     Construct fine set of layers
      QB=BF1                    ! first layer of a set
      J=1                       ! virtual layer
 30   FORMAT(A,2I4,F9.5,G13.5)
      DO I=1,N1FP                ! each fine layer
        BLAF(I)=BLAY(J)*QB      ! thickness of fine layer
        DENF(I)=DENN(J)         ! keep density of original layer
        CTF(I)=CTT(J)           ! " specific heat " "
        KTF(I)=KTT(J)           ! " conductivity " " 
        DIFFI(I)=KTF(I)/(DENF(I)*CTF(I)) ! layer diffusivity
        IF (IDB5.GE.6) WRITE(IOPM,30),'I,j..',I,J,QB,DIFFI(I)
        IF (MOD(I-1,KFL).EQ.0)  THEN ! at the last of a set
          J=J+1                 ! next original layer
          QB=BF1            
        ELSE
          QB=QB*RLAF            ! increase layer factor
        ENDIF              
      ENDDO
      BLAF(1)=BLAF(2)/RLAF      ! thickness of virtual layer
      XCEF(1)=-BLAF(1)/2.D0       ! x is depth to layer center, [cm]
      SCONVF(1)=0.
C      XCEF(2)=BLAF(2)/2.0 ! center of first physicel sub-layer
      DO I=2,N1F                ! each physical fine layer
        XCEF(I)= XCEF(I-1)+ (BLAF(I)+BLAF(I-1))/2.D0 ! center depth
        SCONVF(I)=BLAF(I)**2/(2.D0*DTIM* DIFFI(I)) ! Safety factor
      ENDDO
      WRITE(IOSP,31)'TFINE layers: Num,lowest center[m]',N1F,XCEF(N1F)
 31   FORMAT(A,I5,F10.4)
      LALCON=.NOT. LKOFT
      IF (LKOFT) THEN 
        WRITE(IOSP,32)'T-dep. layers in TDAY :',IKK
 32     FORMAT(A,4I5)
        IK1=KFL*(IKK(1)-1)+1    ! first of above  T-dep material
        IK2=KFL*IKK(2)          ! num fine layers of above tdep material
        IK3=KFL*(IKK(3)-1)+1    ! first of below material
        IK4=KFL*IKK(4)          ! num fine layers of below  T-dep material
        WRITE(IOSP,32)'T-dep. layers in TFINE:',IK1,IK2,IK3,IK4
      ENDIF

C----------------------------------

C     2016 Feb 16
C     Compute the safety factor for each layer prior to any
C     time doubling. Starting from the bottom layer, form the minimum safety 
C     factor at each layer or below. Then from top down, apply time doubling.
C     
      QQ=SCONVF(N1F)            ! minimum safety factor at each layer or lower
      ZDZ(N1F) = QQ             ! " " at each layer. Temporary use of  ZDZ  
      K=N1F                     ! will be layer with lowest safety factor   
      DO J=N1F-1,2,-1            ! LAYER LOOP 2: minimum here or below
        IF (SCONVF(J).LT.QQ) THEN 
          QQ= SCONVF(J)
          K=J
        ENDIF
        ZDZ(J) = QQ !  minimum safety factor at this layer or lower
      ENDDO
      IF (IDB5.GE.1) WRITE(IOSP,'(a,i3,f12.3)') 
     & 'Min safety: layer,factor=',K,QQ
      IF (QQ.LT. ARC3) THEN     ! Some layer unstable
        IRET=-3                  ! error return if unstable
        WRITE (IOERR,137)K,SCONVF(K)
 137    FORMAT ('TFINE: UNSTABLE; Layer, factor =',I3,F8.4)
      ENDIF
C                   all the Qn are available for use
      K=0                       ! number of time doublings so far
      IH=0                      ! will hold total of layers*time
      KKF=N2F                    ! current number of times steps per day 
C  MAXBOT is in parameter common krcc8m 
      KM= MIN(MAXBOT,IFIX(ALOG(FLOAT(N2F))/ALOG(2.)+.001) ) -1
      QA=1.D0                   ! effect on safety factor of K doublings
      DTIMI=DTIM                ! current time step
      DO J=2,N1F                 ! LAYER LOOP 3: Assign time doubling
        IF (K.LT.KM .AND. J.GT.3 .AND. MOD(KKF,2).EQ.0 
     &    .AND. ZDZ(J).GT. 2.D0*QA*CONVF) THEN ! double time step for this layer
          DTIMI=2.*DTIMI        ! doubled time interval
          K=K+1                 ! have new binary interval
          N1Z(K)=J-1            ! bottom layer of prior interval
          KKF=KKF/2          ! number of time steps for this and deeper layers
          QA=QA*2.              ! update doubling factor
        ENDIF
        IF (IRET.LT.1 .OR. IDB5.GE.2) WRITE(42,'(A,I4,2G12.5,F8.1)')  !
     &  'J,BLAF,SCONVF,QA',J,BLAF(J),SCONVF(J),QA        ! print all layers
        SCONVF(J)=SCONVF(J)/QA  ! safety after time doubling
        IH=IH+KKF               ! add timesteps at this depth
C for constant conductivity
        FCJ= 2.D0* DTIMI /(DENF(J)*CTF(J) * BLAF(J)**2) !
        FA1(J)=FCJ*KTF(J)/(1.D0+(BLAF(J+1)*KTF(J)/(BLAF(J)*KTF(J+1))))
        FA3(J)= (BLAF(J)/KTF(J) + BLAF(J+1)/KTF(J+1))
     &        / (BLAF(J)/KTF(J) + BLAF(J-1)/KTF(J-1))
        FA2(J)=-1.D0-FA3(J)
C T-dependent conductivity
        FBI(J)=  BLAF(J+1)/BLAF(J) ! 
        FCI(J)= 2.D0* DTIMI /(DENF(J) * BLAF(J)**2) !
      ENDDO                     ! end of layer loop
      FA1(1)=FA1(2)             !| never used, just for better
      FA3(1)=FA3(2)             !| plot of fort.43
C
      KKF=K+1                   ! one larger than number of time doublings
      N1Z(KKF)=N1F
      WRITE(IOSP,156) (N1Z(K),K=1,KKF)
 156  FORMAT (' TFINE low lay of time doubli: ',15I3)
C set last layer for each time.  KJ(JJ)=K for  JJ time increment
      II=1                      ! spacing
      DO K=1,KKF                ! each doubling
        I=N1Z(K)                ! bottom layer for that doubling
        DO JJ=II,N2,II          ! each time-step along current spacing
          KJ(JJ)=I              ! set the deepest layer to do
        ENDDO
        II=II+II                ! double the spacing
      ENDDO

      JBE(1)=0                  ! need only JBE 1:2 for maximum eclipse possible
      PARI(1)=N2
      PARI(2)=IOERR
      CALL ECLIPSE (PARC,PARI, JBE, FINSOL)
      WRITE(IOSP,243) -778,NCASE,J5,J4,J3,JBE(1),JBE(2),N1F
 243  FORMAT(9I7, L4)
      I=(2*JBE(4)-JBE(3))*KFT ! expected size of  FINSOL 
      IF (I.GT.MAXN2) THEN 
        WRITE(IOERR,*)' Forecast size of ftime exceeds MAXN2'
        IRET=-8
        GOTO 9
      ENDIF

C BIN5 file: vvvvvvvvvvv setup  vvvvvvvvvvvvvvvvvvvvvvv
      CALL FILLD (0.D0,CCC,MAXCCC) ! zero the entire storage array
      CALL FILLL (0,J79,MAXJ79) ! zero the eclipse range
      j79(1)=N2                 ! Save for convenience  N2
      j79(2)=KFT                ! and fine-time factor 
      CALL FILLL (0,JJJ,10)     ! initiate bin5 control 
      NEEP=MIN(MAXCK*KFL,MAXFK-2) !  number of layers to store
      JJJ(1) = 3                ! # of dimensions
      JJJ(2) = 2+NEEP           ! depth:  FSOL+FINSOL+TTF(1:neep)
C ctime steps= J9-J7P +1 = (J8+ J8-J7)-(J7+1) +1 = 2(J8-J7)
      JJJ(3) = 2*(JBE(2)-JBE(1))*KFT ! max fine time steps thru eclipse+follow-on
C      JJJ(4) = N4               ! number of latitudes with eclipse
      JJJ(8) = 5                ! set type as  REAL*8
      MM3=JJJ(2)*JJJ(3)         ! number of words for each latitude
      NECL=0                    ! number of eclipses stored so far

      CASE(2) !========================= day computations  (IQ = 2) ===========
C  Should be called here once for each latitude that has eclipse
      IRET=JLOW
C      LATM=PTOTAL.GT.1.D0       ! atmosphere present flag
      FAC3S = 1.D0-SALB         ! spherical absorption
      FAC4  = 1.D0+1.D0/RLAF
      FAC5  = SKYFAC*EMIS*SIGSB
      FAC45 = 4.D0*FAC5
      FAC6  = SKYFAC*EMIS
      FAC6F = SKYFAC*FEMIS      ! if frost
      FAC7  = KTF(2)/XCEF(2)    ! will be redone if not LALCON

      JBE(1)=1                  ! need FINSOL
      PARI(1)=N2
      PARI(2)=IOERR
      CALL ECLIPSE (PARC,PARI, JBE, FINSOL)
      IF (FINSOL(1) .LT. 0.) THEN ! ERROR
        WRITE(IOERR,*)' ECLIPSE returned error'
        IRET=-7
        GOTO 9
      ENDIF
      J7P=JBE(3)+1              ! index of time step containing eclipse start
      J8=JBE(4)                 ! index of time step containing eclipse end
      J9=J8+ J8-JBE(3)          ! add followon the length of eclipse
      I=(NECL+1)*MM3         ! last storage index possible for current latitude
      LATOK = (I .LE. MAXCCC)   ! ensure enough remaining room
      WRITE(IOSP,243) -777,NCASE,J5,J4,J3,JBE(3),JBE(4),J9,N1F,LATOK
      IF (LATOK) THEN           ! store this eclipse latitude
        NECL=NECL+1             ! 1-based index of this eclipse  within a case
        I=3*NECL                ! used thus far, first 3 are global
        J79(I+1)=J7P-1          ! save tfine-range for this latitude
        J79(I+2)=J9             ! "
        J79(I+3)=J4             ! latitude index
      ENDIF
C interpolate input temperature profile onto the fine layers
      CALL MVD (TTJ,TRET,N1+1)  ! copy  TDAY profile in case too few fine layers 
      DELT=XCEN(1)              ! store virtual layer depth 
      XCEN(1)=0.                ! depth of  TTJ(1)=TSUR
      TSUR=TTJ(1)               ! extract Tsurf from transfer array
      IF (LVFT) THEN            ! overload a logical flag
        CALL DSPLINE (XCEN,TTJ,N1, HUGE,HUGE,YTF) ! first 3 args from common
        DO I=2,N1F              ! each fine layer
          CALL DSPLINT (XCEN,TTJ,YTF,N1,XCEF(I),QA)
          TTF(I)=QA             ! interpolated temperature 
        ENDDO
      ELSE
C 2018feb02 Found that spline interpolation could oscillate near the surface
C so use conservative linear interpolation [for the top few intervals]
C      I=3*KFL ! instead of  N1F
        CALL ORLINT8 (N1,XCEN,TTJ,N1F,XCEF, TTF)
      ENDIF
      XCEN(1)=DELT              ! restore virtual layer depth 

 22   FORMAT(99F8.3)
 23   FORMAT(99G12.4)
      IF (IDB5.GE.4) THEN
        WRITE(43,*)'N1...YTF',N1,FLAY,RLAY, KFL,N1F,RLAF
        WRITE(43,23)(XCEN(I),I=1,N1)
        WRITE(43,22)(TTJ(I),I=1,N1)
        WRITE(43,23)(YTF(I),I=1,N1)
        WRITE(43,23)(BLAY(I),I=1,N1)
        WRITE(43,23)(BLAF(I),I=1,N1)
        WRITE(43,23)(XCEF(I),I=1,N1)
        WRITE(43,*)'C_END'
        WRITE(43,23)(XCEF(I),I=1,N1F)
        WRITE(43,22)(TTF(I),I=1,N1F)
        WRITE(43,23)(FA1(I),I=1,N1F)
        WRITE(43,23)(FA3(I),I=1,N1F)
      ENDIF
C      CALL MVD (XCEF,CCC(3),NEEP) ! store layer central depths
      DELBOT= (TTJ(JLOW+1)-TTJ(JLOW))* ! heat-flow at fine-layer base
     &     ((BLAF(N1FP)-BLAF(N1F))/(BLAY(JLOW+1)-BLAY(JLOW)))
      IF (IDB5.GE.3) WRITE(IOSP,'(a,2f12.4,g13.5)') 
     & 'TTJ(1)...',TTJ(1),TTF(1),DELBOT
      TTF(N1FP)=TTF(N1F)        ! set the lower bowndary
C If self heating, as before v3.4, factors are to the open sky
C If using fff, -- are hemisphere , --P are back-radiation from far ground
C SKYFAC is fractional normalized irradiance from the sky; 1.0 for flat
C SKYFAC is computed in TLATS and arrives thru KRCCOM
      IF (LOPN3) THEN           !F using fff
        FAC5  = EMIS*SIGSB      !F --X far (eXterior) factors are 0 for flat
        FAC45= 4.D0*FAC5        !F
      ENDIF                     !F
      LSELF=.NOT. LOPN3         !F self heating
      LFIN=.TRUE.               ! FINSOL begins in first ctime interval
      FAC8=EMTIR*EMIS
      LALCON = (IK2+IK4 .EQ. 0) ! all Tcon, not Tdep
      IF (IDB5.GE.2) WRITE(IOSP,119) LZONE,LALCON,j5,IK1,IK2,IK3,IK4
 119  FORMAT('LZONE,LALCON,J5, IK1:4=',2L3,5I6)
      JLS=KFT/2 +1                 ! near middle of a coarse time interval
      QA=0.5                    ! if  KFT is even
      IF (MOD(KFT,2).EQ.1) QA=1. ! if  KFT is odd
      DO JL=1,KFT               ! time linear interpolation factors
        TIFAC(JL)=(DBLE(JL)-QA)/DBLE(KFT)
      ENDDO
C initiate radiation for first ctime interval
      JJ=J7P-1                  ! start of ctime one before eclipse start
      JGU=JLS-KFT               ! global fine time when radiation updated, negative
      FALB0=ALBJ(JJ)            ! hemispheric albedo.  base and 
      FALBD=ALBJ(JJ+1)-FALB0    ! slope pairs for fine time interpolation 
      FSOL0=ASOL(JJ)            !  Direct solar flux on sloped surface
      FSOLD=ASOL(JJ+1)-FSOL0
      FFAR0=FARAD(JJ)           ! far-field radiance
      FFARD=FARAD(JJ+1)-FFAR0
      FPLA0=EMIS*PLANH(JJ)+FAC3S*PLANV(JJ) ! combine  IR and visual load
      FPLAD=EMIS*PLANH(JJ+1)+FAC3S*PLANV(JJ+1)-FPLA0
      FDIF0=SOLDIF(JJ)          !  Solar diffuse (with bounce) insolation
      FDIFD=SOLDIF(JJ+1)-FDIF0 

      JFI=0                     ! fine time steps (global) so far
      DO 270 JJ=J7P,J9          ! v+v+v+v+v+ orig. time loop +v+v+v+v+v+v+v+
        FINSJ=1.                ! in case  FINSOL not used
        IF (JJ.GT.J8) LFIN=.FALSE.!   FINSOL extends through  J8=JBE(4) 
        DO 250 J1=1,KFT         ! v.v.v.v.v.v.v fine time loop v.v.v.v.v.v.v.
          IF (J1.EQ.JLS) THEN   ! reset to next coarse radiation interval
            JGU=JFI+1           ! global fine time when radiation updated
            FALB0=ALBJ(JJ)      ! hemispheric albedo.  base and 
            FALBD=ALBJ(JJ+1)-FALB0 ! slope pairs for fine time interpolation 
            FSOL0=ASOL(JJ)      !  Direct solar flux on sloped surface
            FSOLD=ASOL(JJ+1)-FSOL0
            FFAR0=FARAD(JJ)     ! far-field radiance
            FFARD=FARAD(JJ+1)-FFAR0
            FPLA0=EMIS*PLANH(JJ)+FAC3S*PLANV(JJ) ! combine  IR and visual load
            FPLAD=EMIS*PLANH(JJ+1)+FAC3S*PLANV(JJ+1)-FPLA0
            FDIF0=SOLDIF(JJ)    !  Solar diffuse (with bounce) insolation
            FDIFD=SOLDIF(JJ+1)-FDIF0 
          ENDIF
          JFI=JFI+1             ! output index = global fine time
C linear interpolation in time for the radiation fields
          JL=JFI-JGU +1            ! ftime offset from radiation update, index
          FAC3  = 1.D0-(FALB0+TIFAC(JL)*FALBD) ! accomodate photometric functions
          FDIF=FDIF0+TIFAC(JL)*FDIFD ! diffuse flux
          ZFAR=FFAR0+TIFAC(JL)*FFARD ! far-field radiance.  FFAR is assigned
          FPLA=FPLA0+TIFAC(JL)*FPLAD ! planetary heat load
          FSOL=FSOL0+TIFAC(JL)*FSOLD ! collimated insolation onto slope surface
C Set the boundary conditions
          TTF(1)=TTF(2)-FAC4*(TTF(2)-TSUR) ! set virtual layer
          IF (LBASE) TTF(N1FP)=TTF(N1F)+DELBOT ! base heat-flow, constant
          KN=KJ(JFI)             ! depth for this time interval
C
C  -v-v-v-v-v-v-v-v-v-v-v-v-v-v-v- layer loops v-v-v-v-v-v-v-v-v-v-v-v-v-
C     
          IF (LALCON) THEN      ! all layers T-con  --------------
            DO  J=2,KN
              DTJ(J)=FA1(J)* (TTF(J+1)+FA2(J)*TTF(J)+FA3(J)*TTF(J-1)) ! diffusion
            ENDDO 
          ELSE                  ! some T-dep layer ----------------------
C Could here add logic to only compute layers that are used in this time step
            IF (IK2.GT.0) THEN      ! Upper material values
C     EVMONO3D loads arg2 output values into locations starting at last arg.
              CALL EVMONO3D(CCKU,IK2,TTF(IK1), KTF(IK1)) ! upper Tdep | k
              CALL EVMONO3D(CCPU,IK2,TTF(IK1), CTF(IK1)) ! material   | Cp
            ENDIF
            IF (IK4.GT.0) THEN      ! There are lower material layers
              CALL EVMONO3D(CCKL,IK4,TTF(IK3), KTF(IK3)) ! lower Tdep | k
              CALL EVMONO3D(CCPL,IK4,TTF(IK3), CTF(IK3)) ! material   | Cp
            ENDIF

            FBK=RLAF            ! F_B_i * F_k_i for virtual layer
            DO J=2,KN           ! kt
              FBKL=FBK          ! kt
              FBK= FBI(J)*KTF(J)/KTF(J+1) ! F_B_i * F_k_i 
              FA1J=FCI(J)*KTF(J)/(CTF(J)*(1.+FBK)) ! eq F1
              FA3J=(1.D0+FBK)/(1.D0+1.D0/FBKL) ! eq F3
              DTJ(J)=FA1J*(TTF(J+1)-(1.D0+FA3J)*TTF(J)+FA3J*TTF(J-1)) ! diffusion
            ENDDO               ! kt
            FAC7=KTF(2)/XCEF(2)  ! do each time because  KTF(2) might be  T-dep.
          ENDIF                 !---------------------- 
C     - - - - - - - - - - - - -
          DO  J=2,KN
            TTF(J)=TTF(J) + DTJ(J) ! apply the delta-T
          ENDDO
C     
C     -^-^-^-^-^-^-^-^-^-^-^-^-^-^-^- end of layer loops ^-^-^-^-^-^-^-^-^-^-^
C  upper boundary conditions.
          II=0                  !db  Newton iteration count
          IF (LFIN) FINSJ=FINSOL(JFI) !  ECLIPSE results
          ABRAD=FINSJ*(FAC3*FSOL+FAC3S*FDIF) ! surface absorbed radiation
          IF (LPH) ABRAD=ABRAD+FPLA ! planetary IR and VIS flux absorbed.

 230      TS3=TSUR**3           ! bare ground
          II=II+1
          SHEATF= FAC7*(TTF(2)-TSUR) ! upward heat flow to surface
          POWER = ABRAD + SHEATF - FAC5*TSUR*TS3 ! unbalanced flux
          IF (LOPN3) POWER=POWER+ZFAR ! fff only
          DELT = POWER / (FAC7+FAC45*TS3)
          TSUR=TSUR+DELT
          IF (MOD(II,10).EQ.0) WRITE(IOPM,*)J5,J4,J3,JJ,II,TSUR,DELT !db
          IF (TSUR.LE. 0. .OR. TSUR.GT.TBLOW) GOTO 340 ! instability test
          IF (ABS(DELT).GE.GGT) GOTO 230 ! fails convergence test
          TSUR4=TSUR**4
          TTF(1)=TSUR

          IF (IDB5.GE.6 .AND. (JJ.LT.(J7P+3)  .OR. ABS(JJ-J8).LT.3))
     &       WRITE(44,244) JFI,FINSJ,TSUR,ABRAD,SHEATF,POWER,FAC7,KN 
 244      FORMAT(I6,f7.4,F8.3,3f11.5,g12.5,i4)
D         WRITE(44,245) JFI,FINSOL(JFI), (TTF(I),I=1,N1F)
D 245     FORMAT(I6,F8.4,F8.3, 99F7.2)
C BIN5 file: vvvvvvvvvvv store  vvvvvvvvvvvvvvvvvvvvvvv
C  CCC is [2+depth, fine-time,latitude]
          IF (LATOK) THEN       ! store this eclipse latitude
            KC=(NECL-1)*MM3+(JFI-1)*JJJ(2) ! words already used
            CCC(KC+1)= FSOL     ! direct insolation
            CCC(KC+2)= FINSJ    ! insolation factor
            CALL MVD (TTF,CCC(KC+3),NEEP) ! move layer temperatures to storage
          ENDIF
C     ^^^^^^^^^^^^^^ end bin5 stuff ^^^^^^^^^^^^^^

 250    CONTINUE                !^.^.^.^.^.^.^. end of fine time loop ^.^.^.^.^

 270  CONTINUE                  !^+^+^+^+^+^+^+ end of time loop ^+^+^+^
 

C interpolate fine temperature profile onto the  TDAY layers
C  For  KFL odd, could copy a fine layer  T,  but still would need some kind 
C of interpolation code for  KFL even. Simpler to use splines for both.
      DELT=XCEF(1)  ! store fine virtual layer depth 
      XCEF(1)=0.    ! depth of ttf(1)=tsur
      TTF(1)=TSUR
      IF (LVFT) THEN  ! overload a logical flag
        CALL DSPLINE (XCEF,TTF,N1F, HUGE,HUGE, YTR) ! get the derivatives
        DO I=2,JLOW             ! each fine layer
          CALL DSPLINT (XCEF,TTF,YTR,N1F,XCEN(I), QA) ! spline interpolation
          TRET(I)=QA            ! interpolated temperature 
        ENDDO
      ELSE
        CALL ORLINT8 (N1F,XCEF,TTF,JLOW,XCEN, TRET)
      ENDIF
      XCEF(1)=DELT              ! restore fine virtual layer depth 
      TRET(1)=TSUR              ! transfer surface temperature
      IF (IDB5.GE.4) THEN
        WRITE(47,22)(TTF(I),I=1,N1F) ! fine  T
        WRITE(47,22)(TRET(I),I=1,JLOW) ! coarse  T
      ENDIF

      CASE(3) ! ================== BIN5 file write =============================
      IF (NECL.GT.0) THEN
        JJJ(4) = NECL           ! number of latitudes with eclipses
        I=NCASE/10              !| convert case number to string
        J=NCASE-10*I            !| assumes it ie 99 or less
        WRITE(BUFF,'(2I1)')I,J
        I=LNBLNK(FRUN)
        FUFF=FRUN(1:I)//'tfine'//BUFF//'.bin5' ! output file name
        I=3*(1+NECL)                ! number defined for latitudes
        CALL STRUMI(J79,I,'^',HEADTX,K) ! make one string
        HEADTX=VERSIN//' TFINE output N2,J7P:9='//HEADTX(1:K) ! 
        CALL STRUMR8(PARC,12,'!',BONG,K) ! make one string
        I=LEN_TRIM(HEADTX)      ! index of last non-blank character 
        HEADTX=HEADTX(1:I)//'  PARC='//BONG(1:K) !
        CALL STRUMR8(XCEF,NEEP,'`',BONG,K) ! make one string
        I=LEN_TRIM(HEADTX)      ! index of last non-blank character 
        HEADTX=HEADTX(1:I)//'  depths='//BONG(1:K) !
        I=LEN_TRIM(HEADTX)      ! index of last non-blank character  
        JJJ(9)=I                ! length of header
        WRITE(IOSP,603)NCASE,JJJ
 603    FORMAT ('TFINE: Case=',I2,'  JJJ=',10I5)
        I=IDB3                  !  +1=report many values   +2 report progress 
        CALL BINF5 ('W',FUFF,HEADTX,JJJ,CCC,I) ! 
        WRITE (IOSP,*)'TFINE wrote ',FUFF,'  iret= ',I
      ELSE
        WRITE (IOSP,*)'TFINE: No latitudes had eclipses' 
      ENDIF
      CASE DEFAULT ! ==================
        STOP ! should never call with other IQ 
      END SELECT ! ==================
C
 9    IF (IDB5.GE.1) WRITE(IOSP,*) 'TFINE exit'
      RETURN
C
 340  IRET=-2                  !  blow-up. force a stop; print current conditions
      WRITE(IOSP,*)'TFINE blowup: JJ,J3,J4,J5=',JJ,J3,J4,J5
      WRITE(IOSP,*)'LOPN3,Tsur=',LOPN3,TSUR
      WRITE(IOSP,*)'FARAD,SHEATF,POW,DT=',FARAD(JJ),SHEATF,POWER,DELT
      CALL TPRINT8 (7)           ! print message and  TTJ
      WRITE(IOSP,*) 'TFINE: DELT,TBLOW=',DELT,TBLOW
      CALL TPRINT8 (4)           ! print daily convergence
      GOTO 9

      END
