      SUBROUTINE TIFOV (ALAT,HOUR,PRES,TAUR, TOUT)
C_Titl  TIFOV:  interpolate thermal models to a location in 4 dimensions
      IMPLICIT NONE
C_Vars
	INCLUDE 'modcom.inc'
	INCLUDE 'errcom.inc'
C_arg
      REAL*4 ALAT	!in. request latitude, degrees
      REAL*4 HOUR	!in. request time of day; 24 hours per solar day
      REAL*4 PRES	!in. request pressure, per modcom.inc. -= use default 
      REAL*4 TAUR	!in. request atmospheric dust opacity; -= use default
      REAL*4 TOUT(2*NIA)!out. model temperatures for all inertia and albedos 
C			at the specified location, pressure and opacity
C  First  NIA are for the surface, 2nd  NIA are for the top-of-atmosphere.
C  Upon initial call,  TOUT(1) <0. means there was an error in pressure (-1)
C    or opacity (-2) values in  COMMOM.
C_lims
C  Assumes that models have uniform increments in latitude, hour.
C  Assumes the model sequence is equivalent to indexes of (i&a,tau,p)
C   and that  P models are  [low,medium=default,high]
C   and that tau models are [low=default,medium,high]
C  Self-initializes only once, so conditions must not be changed.
C_Desc
C limit checking could be simplified, but would be slower for normal case.
C  This method assumes that all models in the 4-dimensional space have been 
C computed and are available.  This is bicubic interpolation in hour/latitude
C and optionally quadratic in opacity and pressure.
C
C  First,  THLM models are interpolated in latitude and hour into  
C a one-dimensional array  TP, with one element for each model.
C  Then,  TP is interpolated in opacity, replacing the first opacity
C value with the interpolation result, a temperature; this leaves the useful
C values for the first pressure as the first  NIA (= the number of
C inertia-albedo models) values, and the other 2 pressure sets spaced out by
C  NIT (=  NIA times the number of opacity models  NTAU).
C_Calls  R2R  SPLINT1  SPLINT2  SPLINT4
C_History
C  97sep09  Hugh_Kieffer  USGS_Flagstaff
C  98apr24  HHK convert to bi-cubic splines for hour/latitude   may22 debug
C  98jun29  HHk normaalize pressure for the quadratic interpolation

C_End 789012345678901234567890123456789012345678901234567890123456789012_4567890
        INTEGER KHLO /1/    ! assign error counter for invalid low hour
        INTEGER KHHI /2/    ! assign error counter for invalid high hour
        INTEGER KLLO /3/    ! assign error counter for invalid low latitude
        INTEGER KLHI /4/    ! assign error counter for invalid high latitude
        INTEGER KPLO /5/    ! assign error counter for low pressure
        INTEGER KPHI /6/    ! assign error counter for high pressure
        INTEGER KTLO /7/    ! assign error counter for low opacity
        INTEGER KTHI /8/    ! assign error counter for high opacity
      LOGICAL LFIRST /.TRUE./
      REAL*4 TP(NMOD2)           ! all models at the specific lat/hour
      LOGICAL LTAUP             ! true if interpolating in tau and pres

      INTEGER J,J2,K,K1,K2,KO,KT,  NIT,NIT2   ! local variables
      REAL*4  Y, XR,YR, X21,X31,X32,Y21, F1,F2,F3,F4 ! fractions & factors
      REAL*4 HOUR0,ALAT0,HLAST, TAURL,TAURH,PRESL,PRESH ! model limits
      REAL*4  TF1,TF2,TF3,TF4 ! fractions & factors

      REAL*4 TL(4),DT,DP,DTSQ,DPSQ, BB,CC
      REAL*4 FOUR(4) /1.,2.,3.,4./ ! uniform x for latitude cubic fit

      SAVE HOUR0,ALAT0,HLAST,NIT,NIT2,LTAUP,LFIRST
      SAVE TAURL,TAURH,TF1,TF2,TF3,TF4
      SAVE PRESL,PRESH, F1, F2, F3, F4
C=============== do initialization only once ===========================
      IF (LFIRST) THEN
         tout(2)=1              ! version of this routine
        HOUR0=HOUR1-HOURDEL       ! base for hour system
        ALAT0=ALAT1-ALATDEL       ! base for latitude
        HLAST=HOUR0+NHOUR*HOURDEL ! hour of last array point
        DO J=1,NHP1               ! load the hour array for spline routines
           HOURS(J)=HOUR0+(J-1)*HOURDEL
           ENDDO
        NIT = NIA*NTAU            ! # of models in a pressure set
        NIT2 = NIT+NIT
        LTAUP = NPRES*NTAU.GT.1   ! will do interpolation in tau and pres.

        IF (NTAU.GT.1) THEN
          TAURL=VT(1)           !  low-limit for opacity
          TAURH=VT(3)           ! high-limit for opacity
C opacity quadratic interpolation factors
          X21 = VT(2)-VT(1)     ! pre-compute the denominators
          X32 = VT(3)-VT(2)
          X31 = VT(3)-VT(1)
          PRESL=VP(1)           !  low-limit for pressure
          PRESH=VP(3)           ! high-limit for pressure
C-----everything above here depends upon   modcom.inc ---------
          IF (X21.EQ.0. .OR. X32.EQ.0. .OR. X31.EQ.0.) THEN
            TOUT(1)=-1.
            RETURN
          ENDIF
          TF1 =  1./(X31*X32)    ! compute constant terms involving x
          TF2 = -1./(X21*X32)
          TF3 = 1./X21
          TF4 = -X21
        ENDIF
        IF (IDB2.GE.0) WRITE(IDB2,*)'X&Fs ',X21,X31,X32,TF1,TF2,TF3,TF4

        IF (NPRES.GT.1) THEN
C pressure quadratic interpolation factors, normalize  P to middle value
          X21 = (VP(2)-VP(1))/VP(2)     ! pre-compute the denominators
          X32 = (VP(3)-VP(2))/VP(2)
          X31 = (VP(3)-VP(1))/VP(2)
C          x21 = vp(2)-vp(1)     ! pre-compute the denominators
C          x32 = vp(3)-vp(2)
C          x31 = vp(3)-vp(1)

          IF (X21.EQ.0. .OR. X32.EQ.0. .OR. X31.EQ.0.) THEN
            TOUT(1)=-2.
            RETURN
          ENDIF
          F1 =  1./(X31*X32)    ! compute constant terms involving x
          F2 = -1./(X21*X32)
          F3 = 1./X21
          F4 = -X21
        ENDIF
        IF (IDB2.GE.0) WRITE(IDB2,*)'X&Fs ',X21,X31,X32,F1,F2,F3,F4
        LFIRST=.FALSE.          ! don't do this again
        RETURN
      ENDIF
C======================================= done initialization ===============

C------------ process hour/latitude ---------------------
      Y=(ALAT-ALAT0)/ALATDEL    ! find index and fraction for latitude
      J2=INT(Y) ! should be 2nd of 4 latitudes used for bi-cubic
C below, if  J2-band is next to a pole, the point 'beyond' the pole will be used
      IF (J2.LT.1) THEN          ! latitude was too small
        KERR(KLLO)=KERR(KLLO)+1 ! increment error count
        Y=1.0                   ! effectively set latitude to lower valid limit
        J2=1
      ELSEIF (J2.GE.NLAT) THEN       ! latitude was too high
        KERR(KLHI)=KERR(KLHI)+1 ! increment error count
        Y=REAL(NLAT)-.01        ! effectively set latitude to upper valid limit
        J2=NLAT-1               ! will yield  YR=2.99
      ENDIF
      YR=Y-(J2-2)! point to middle interval of 4-point set

      XR=HOUR
      IF (XR.LT.HOUR0) THEN
         KERR(KHLO)=KERR(KHLO)+1 ! increment error count
         XR=0.
      ELSEIF (XR.GT.HOURS(NHP1)) THEN
         KERR(KHHI)=KERR(KHHI)+1 ! increment error count
         XR=HOURS(NHP1)
      ENDIF
      CALL SPLINT1 (HOURS,NHP1,XR) ! set interpolation point and constants

C Interpolate all models to the latitude/hour location, bi-cubic splines
C Code for near poles assumes that the extreme latitudes are 1/2 of 
C the latitude interval from the pole.
      DO K=1,NMOD2              ! interpolation 4 latitudes to specific hour
         CALL SPLINT2 (THLM(1,J2,K),DHLM(1,J2,K),TL(2)) ! lower lat.
         J=J2+1
         CALL SPLINT2 (THLM(1,J,K),DHLM(1,J,K),TL(3)) ! upper lat
         IF (J2.LT.2) THEN      ! extend over the pole
C            TL(1)=TL(3) ! 2008jan30 ERROR, should have been TL(2)
            TL(1)=2.*TPOLE(1,K)-TL(2) ! extrapolate, to keep uniform spacing
         ELSE
            J=J2-1
            CALL SPLINT2 (THLM(1,J,K),DHLM(1,J,K),TL(1)) ! lowest lat
         ENDIF
         IF (J2+2.GT.NLAT) THEN ! extend over the pole
C            TL(4)=TL(2) ! 2008jan30 ERROR, should have been TL(3)
            TL(4)=2.*TPOLE(2,K)-TL(3)
         ELSE
            J=J2+2
            CALL SPLINT2 (THLM(1,J,K),DHLM(1,J,K),TL(4)) ! highest lat
         ENDIF
c now have 4  T's in adjacent latitudes, need to interpolate in middle interval
         CALL SPLINT4 (FOUR,TL,YR,TP(K))
      ENDDO

C  TP is now: (nia, 3 for tau, 3_for_pressure, 2 for surface/atm)


      IF (LTAUP) THEN   ! ======= will do  4-D interploation =============
C--------------- process opacity --------------------------
C the base (default) model has been replaced with the interpolated value
C  Thus, if caller requests default,  don't need to do anything.
        IF (TAUR.GE.0) THEN ! caller wants specific value
C interpolate in tau; models are (i&a, 3_for_tau, 3_for_pres, surf&atm)
          DT=TAUR-VT(1)         ! offset for quadratic, opacity
          DTSQ=DT**2
          IF (TAUR.LT.TAURL) THEN ! test input value; too small
            KERR(KTLO)=KERR(KTLO)+1 ! increment error count
          ELSEIF (TAUR.GT.TAURH) THEN ! value was too high
            KERR(KTHI)=KERR(KTHI)+1 ! increment error count
          ENDIF
C NIA is  #inertia * #albedo      NIT is NIA * #opacity
          DO J=1,NMOD2,NIT        ! for each pressure set & 2 T sets
            DO K=J,J+NIA-1        ! each inertia/albedo for first opacity
              K2=K+NIA            ! index of middle opacity
              Y21 = TP(K2)-TP(K)  ! first difference of T
              CC = TF1*(TP(K2+NIA)-TP(K)) + TF2*Y21 ! quadratic coefficent
              BB = TF3*Y21 + TF4*CC ! linear coefficent
              TP(K)=TP(K)+BB*DT+CC*DTSQ ! overwrite low value with interpolation
            ENDDO
          ENDDO
        ENDIF
        
C  TP is now: (nia, interp_tau&2, pressure, 2 for surface/atm)
C--------------- process pressure --------------------------
        KO=0
        IF (PRES.GE. 0.) THEN   ! process specific pressure
          DP=(PRES-VP(1))/VP(2) ! offset for quadratic, normalized pressure
C          dp=pres-vp(1) ! offset for quadratic
          DPSQ=DP**2
          IF (PRES.LT.PRESL) THEN ! test input value; too small
            KERR(KPLO)=KERR(KPLO)+1 ! increment error count
          ELSEIF (PRES.GT.PRESH) THEN ! value was too high
            KERR(KPHI)=KERR(KPHI)+1 ! increment error count
          ENDIF
          
C interpolate in pressure; 2nd & 3rd  P's are offset by  NIT models
          DO KT=1,2             ! surface and atm  T sets
            K1=1  +(KT-1)*NMOD  ! first  A/I in first  P set
            K2=K1-1+NIA         !  last  A/I in first  P set
            DO K=K1,K2          !  full set of a/i
              Y21 = TP(K+NIT)-TP(K) ! first difference of T
              CC = F1*(TP(K+NIT2)-TP(K)) + F2*Y21 ! quadratic coefficent
              BB = F3*Y21 + F4*CC ! linear coefficent
              KO = KO+1
              TOUT(KO) = TP(K) + BB*DP + CC*DPSQ ! pressure interpolation
            ENDDO
          ENDDO
        ELSE                    ! use default pressure value
          DO KT=1,2             !  T sets
            K1=NIT+1+(KT-1)*NMOD ! select the second pressure set.
            K2=K1-1+NIA
            DO K=K1,K2          ! 2nd model in pressure is default
              KO=KO+1
              TOUT(KO)=TP(K)    ! transfer the default model to output
            ENDDO
          ENDDO
        ENDIF
        
      ELSE    ! ================ only  2-D interpolation, =============
C TP is (inertia,albedo,1,1,2) = (inertia,albedo,2)
         CALL R2R (TP,TOUT,NMOD2)! copy both T sets
      ENDIF
C------------ debug printout ---------------------------
      IF (IDB2.NE.0) THEN
        WRITE (IDB2,333)ALAT,HOUR,J2,XR,YR
 333    FORMAT('ALAT,HOUR,J2,XR,YR=',2F7.2,I3,2F7.3)
        WRITE (IDB2,334)TAUR,PRES,DT,DP
 334    FORMAT('TAUR,PRES,DT,DP=',F7.2,F7.1,F7.3,F7.1)
      ENDIF

      RETURN
      END
