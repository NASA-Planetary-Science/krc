      REAL FUNCTION ERTIA (TOBS,ALBIN,TP, DIDT)
C_Titl  ERTIA: determine thermal inertia for observed temperature and albedo
      IMPLICIT NONE
C_Vars 
      INCLUDE 'modcom.inc'
      INCLUDE 'errcom.inc'
      INTEGER KERLO /9/  ! error counter for observed  T is below any model
      INTEGER KERHI /10/ ! error counter for observed  T is above any model
      INTEGER KERNO /15/ ! error counter for no  T range in models
C_arg
      REAL*4 TOBS     !in. Observed temperature, negative to initialize
      REAL*4 ALBIN    !in. Observed bolometric albedo. If negative, use default
      REAL*4 TP(NUMI,NUMA)  !in. Model temperatures at the specified location
      REAL*4 DIDT     !out. absolute derivative of ln_inertia wrt temperature
C if a  T interval is found, del_t used is at least 1.e-3
C if  TOBS is outside model range, returns  BIG number (see below)
CC    REAL*4 ERTIA       !func. Thermal inertia determined
C_Desc
C Will use observed albedo if available.
C Note that model temperatures may not be monotonic.
C  Assumes that model are (inertia,albedo)
C  SOME  THINGS  HARD  CODED !!!!!
C  Algorithm:
C  Expects input of temperature at this location for a set of inertias, and a 
C few other values that allow determination of the partial derivative wrt 
C albedo.  If positive albedo is input, uses that; else uses default albedo.
C  Corrects model temperatures to this albedo, resulting in  T for a set of 
C inertias; searches for the inertias with the extreme temperatues, and 
C interpolates between these.  This handles the problem of non-monotonic 
C temperatures, or a narrow temperature range that can occur in twilight.
C  Returned value will always be within the limiting model values.
C  Current algorithm does linear interpolation in  T/Log_I space.
C if models have no  T range, will set inertia to  VI(1)
C
C  MUST call with negative  TOBS after  COMMON is initialized and before called
C with a real temperature, then ignores 2nd and 3 arguments.
C_Calls RNDEX  RTERP
C_History
C  97sep09  Hugh_Kieffer  USGS_Flagstaff original version
C  98may27  HHK expect 3 albedo models, each with full set of inertias
C 2002mar11  HK fix log of ratio  (was log of difference)
C 2010apr06  HK ensure index of  T in bounds 
C_End6789012345678901234567890123456789012345678901234567890123456789012_4567890

      REAL*4 T (NUMI)
      REAL*4 LOGI (NUMI)       ! to hold natural log of model inertias
      REAL*4 LOGR (NUMI)       ! to hold natural log of inertia ratios
      INTEGER I,J,IHI,ILO,I1,N,NP1
      REAL*4 A,ALB,TMIN,TMAX,X,Y
      REAL*4 RNDEX,RTERP        ! functions
      REAL*4 BIG     /99.11/     ! max number for  DIDT when small  T range
      REAL*4 BIGGER  /99.55/     ! big number for  DIDT when outside  T range
      REAL*4 BIGGEST /99.99/     ! big number for  DIDT when no  T range
      SAVE LOGI,LOGR

      IF (TOBS.LE.0) THEN       ! negative  TOBS is flag to initialize
        DO I=1,NUMI
          LOGI (I)=ALOG(VI(I))  ! get log of inertias
          IF (I.LT.NUMI) LOGR(I)=ALOG (VI(I+1)/VI(I)) ! log of ratio
        ENDDO
        ERTIA=1.
        RETURN                  ! and return
      ENDIF
      
C Select the albedo to be used
      ALB=ALBIN
      IF (ALB.LE.0.) ALB=ALBDEF

      J=NUMI
      IF (ALB.LT.VA(2)) THEN 
         N=1  ! use lower set of albedos
      ELSE
         N=2
      ENDIF                   
      NP1=N+1

C Linear interpolation (or extrapolation) in this albedo interval
      Y=(ALB-VA(N))/(VA(NP1)-VA(N))
      X=1.-Y
      DO I=1,NUMI
         T(I)=X*TP(I,N)+Y*TP(I,NP1)
        ENDDO

C Now have  T for each inertia value, but may not be monotonic
C Find the extreme locations
      TMIN = T(1)
      TMAX = TMIN
      ILO=1 ! will become index of  lowest model temperature 
      IHI=1 ! will become index of highest model temperature 
      DO I=2,NUMI
        A=T(I)
        IF (A.LT.TMIN) THEN
          TMIN = A
          ILO=I
        ELSEIF (A.GT.TMAX) THEN
          TMAX = A
          IHI=I
        ENDIF
      ENDDO

C Get first of extremes, and the number contained within the extremes
      IF (IHI.GT.ILO) THEN
        I1=ILO                  ! index of first temperature
        N=IHI-I1+1              ! number including the extremes
      ELSEIF (IHI.LT.ILO) THEN
        I1=IHI
        N=ILO-I1+1
      ELSE                      ! no temperature range amoungst models
        I1=ILO                  ! may occur over polar caps
        N=1
        Y=0
        X=0
        KERR(KERNO)= KERR(KERNO)+1 ! increment error count
        ERTIA=VI(1)-1.            ! arbitrarily return first inertia - 1.
        DIDT=BIGGEST
        GOTO 9
      ENDIF

C  RNDEX &  RTERP work together to return valid interpolation or nearest extreme
      X=RNDEX(TOBS,T(I1),N)     ! gets real index of location of arg1 within arg2
      IF (X.LT.1) THEN          ! some error occured
        IF (X.GE.-1) THEN       ! range was zero, or  TOBS below all models
          KERR(KERLO)= KERR(KERLO)+1
        ELSE
          KERR(KERHI)= KERR(KERHI)+1
        ENDIF
        DIDT=BIGGER
      ELSE
         X=AMIN1(FLOAT(NUMI)-1.E-5,X) ! ensure cannot exceed
        I=X                                ! get interval containing  TOBS
        A = MAX (1.E-3,ABS (T(I+1)-T(I)))  ! insure not too small  2010apr06
        DIDT=MIN( LOGR(I)/A, BIG)            ! derivative in that interval
      ENDIF
      ERTIA= EXP (RTERP(LOGI(I1),X))   ! log interpolation in proper interval

 9    IF (IDB2.NE.0) WRITE (IDB2,333) Y,I1,N,X,ERTIA
 333  FORMAT('ERTIA: y,i1,n,x,ertia=', F7.3,2I4,F7.3,F9.3)

      RETURN
      END
      
      
      
