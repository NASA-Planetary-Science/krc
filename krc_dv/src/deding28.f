        SUBROUTINE DEDING28 (OMEGA,G0,ASUR,COSI,TAU, BOND,COLL,RI)
C_Titl  DEDING2 delta-eddington solution for single homogeneous layer
        IMPLICIT  NONE ! Double precision throughout
C_Arg
        REAL*8 OMEGA    ![i] dust single scattering albedo
        REAL*8 G0       ![i] dust asymmetry parameter
        REAL*8 ASUR     ![i] surface albedo
        REAL*8 COSI     ![i] cosine of incidence angle
        REAL*8 TAU      ![i] dust vertical opacity
        REAL*8 BOND     ![o] planetary (atm plus surface system) albedo
        REAL*8 COLL     ![o] Direct beam at bottom = collimated + aureole
        REAL*8 RI(2,2)  ![o] diffuse irradiances: 
C               (1,=  I0 = isotropic   (2, =  I1 = assymetric
C               ,1)= at top of atmosphere  ,2) = at bottom of atm
C_Desc
C  Assumes unit solar irradiance at top of atmosphere perpendicular to 
C    the direction of incidence. e.g.,  pi*F = 1.
C  {NOTE: this is different than older  DEDING, which used   pi*f = 1/cos_i .}
C  Eddington approximation  I(t,u) =  I0(t) +  u*I1(t).  SW eq.2,
C   where  t = tau = extinction opacity,  and u = cosine(angle from zenith).
C  The diffuse flux is  F =  pi*[I0+-(2/3)*I1]   + is down, - is up.  SW eq. 8.
C  For large tau, which would exceed machine single-precision range,
C   this routine returns the semi-infinite result.
C  Based upon:
C.  JWW =  The  Delta-Eddington  Approximation for radiative flux transfer.
C.  J.H.Joseph,W.J.Wiscombe,J.A.Weinman.  J.Atm.Sci vol.33,2453-2459 (1976)
C.  SW = the transfer of solar irradiance through inhomgeneous turbid
C    atmospheres evaluated by  Eddingtons approximation.
C.  E.P.Shettle,J.A.Weinman.  J.Atm.Sci vol.27,1048-1055 (1970)
C  All equation numbers are from  SW unless otherwise noted.
C
C  Upward diffuse irradiance at the top of atm is:  
C       topdiff=PI*[RI(1,1)-(2./3.)*RI(2,1)]
C  Downgoing diffuse irradiance at the bottom of atm is:  
C       botdiff=PI*[RI(1,2)+(2./3.)*RI(2,2)]
C  Total flux reaching a horizontal surface is: trans=botdiff + cosi*coll
C  Atm. heating is : cosi-topdiff-(1.-ASUR)*trans
C_Limitations
C Limits  OMEGA to .999999 and  |G0| to .99 
C_Calls  None
C_Hist 1982jan--  Davip_A_Paige  RADLIB
C 1986sep28  Hugh_H_Kieffer allow very large tau, gives semi-infinite result
C 1986oct13  HHK  Use determinant in inverting  C1&C2. Add comments.
C 1987jan27  HHK remove debug and unused code.
C 1988may05  HK  Treat unit solar irradiance at top of atmosphere as 
C     perpendicular to the direction of incidencederive from  DEDING.
C 1999apr15  HHK bring  EXPMAX &  VLARGE well within single precision range
C       and test for extreme  G0. 
C  Add conservative scattering case, but it does not give similiar answers.
C  Seems to works consistantly for  OMEGA up to .999999, so use this as limit
C  After messing around with limiting cases for  G0 near +- 1, decide
C  to simply limit |G0| to .99 
C 2002jul16 HK Changed name from DEDING1, 
C       revised fourth and next-to-last argument
C 2002jul16  Using qdeding2.pro, results agree with JWW figures only for BOND.
C 2002jul21  Go to double precision computation; all agree with JWW figs 1,3
C 2012feb26 HK Remove unused variables
C 2014mar10 HK Make version with REAL*8 arguments 
C 2014may04 HK Untabify
C_End
C1      REAL vv(2,2),BB(2)      ! matrix and right-hand side
C1      INTEGER*4 INDX(2)               ! used by ludcmp
        REAL*8 PI/3.141592653589793D0/  ! pi
        REAL*8 ZERO  /0.D0/     ! DOUBLE PRECISION CONSTANTS
        REAL*8 ONE   /1.D0/
        REAL*8 TWO   /2.D0/
        REAL*8 THREE /3.D0/
        REAL*8 FOUR  /4.D0/
C       REAL*8 VLARGE/1.0D200/  ! large real number
        REAL*8 EXPMAX /460.51D0/ ! maximum exponent
        REAL*8 GMAX /0.999D0/   ! maximum magnitude of G
        REAL*8 EPSILON /1.D-8/  ! limit for 1-omega

        REAL*8 A,ALPHA,A0,BETA,CAP,CAPT,C1,C2 
     &,EPKT,EMTC,EMKT,F,FACTOR,F0,G, GIN,OMA, OMF,OMFA0
     &,P,TT,T1,T1A ,VDET,V1,V2,V3,V4,V5,V6,X

        GIN=MAX(MIN(G0,GMAX),-GMAX) ! G can cause blow up; limit assymetry
        A0=     MIN(OMEGA,ONE-EPSILON) ! avoid 0 determinant at omega=1.
        TT=TWO/THREE            ! 2/3   
C  Delta-Eddington transformation
        F = GIN**2              !  JWW eq. 5 for f'
        OMF=ONE-F
        G = (GIN-F)/OMF         !  JWW eq. 2b for g'
        OMFA0=one-F*A0
        T1 = OMFA0*TAU          !  JWW eq. 13 for tau'
        X = T1/COSI             !
        IF (X.LT.EXPMAX) THEN
          EMTC = DEXP(-X)       ! exp (- tau'/mu_0)
        ELSE
          EMTC = ZERO           ! outside machine range,  set=0 
        ENDIF

!  Non-conservative case ----- [ conservative case coded in DEDING1 ]

        A = OMF*A0/OMFA0        !  JWW eq. 14 for omega'
        OMA = ONE-A             ! 1 - scaled albedo (used in 4 places)

C  Eddington 2-stream single homogeneous layer
C note. integral from -1 to +1 of   u*(x*u) du   is 2/3 x.  u = cos theta
        CAP = DSQRT (THREE*OMA*(one-A*G)) ! eq. 12b+1 for kappa_i ->0 as  A0->0
        P = DSQRT (THREE*OMA/(one-A*G))   ! eq. 12b+2 for p_i     ->0 as  A0->0
C if  P=0, then  V1=V2 &  V4=V5 &  VDET=0
C       if (p .lt. 1.e-4) P=1.e-4 ! avoid pole

        F0 = one/PI                             ! unit irradiance
        FACTOR=THREE*A*F0*COSI / (FOUR*(one-(CAP*COSI)**2))! used in next 2 lines
        ALPHA = FACTOR* COSI*(one+G*OMA)                ! eq. 12b+3 for alpha_i
        BETA =  FACTOR* (one+THREE*G*OMA*COSI**2)       ! eq. 12b+4 for beta_i

C set up simultaneous equations
CC [ V1 V2 ] [C1] = [V3]
CC [ V4 V5 ] [C2]   [V6]
        V1 = one+TT*P           ! coefficients of eq. 13
        V2 = one-TT*P
        V3 = ALPHA+TT*BETA
        CAPT = CAP*T1
        IF (CAPT.LT.EXPMAX) THEN        ! within machine floating-point range
                EPKT = DEXP(+CAPT)
                EMKT = DEXP(-CAPT)
                T1A = TT*(one+ASUR)     ! used in 3 places
C v4,v5,v6 are coeff. of eq. 14
                V4 = (one-ASUR-T1A*P)*EMKT      ! can get small
                V5 = (one-ASUR+T1A*P)*EPKT      ! can get large
                V6 = ((one-ASUR)*ALPHA-T1A*BETA+ASUR*COSI*F0)*EMTC

                VDET = V1*V5 - V4*V2            ! determinant of eq. 13 and 14
C determinant goes to  V1*V5 as tau gets large
                C1 = (V3*V5-V6*V2)/VDET
                C2 = (V1*V6-V4*V3)/VDET
C Use LU decomp. to solve. 2002jul20. Confirmed that above C1,C2 correct.
!               vv(1,1)=v1
!               vv(1,2)=v2
!               vv(2,1)=v4
!               vv(2,2)=v5
!               bb(1)=v3
!               bb(2)=v6
!               call ludcmp(vv,2,2,indx,d1) ! preform LU decomposition
!               call lubksb(vv,2,2,indx,bb) ! solve for specific RHS.
!               write(*,*) vdet,c1,c2
!               write(*,*) 'bb=',bb
!               c1=bb(1)
!               c2=bb(2)
c               write(*,*) v1,v2,v3
c               write(*,*) v4,v5,v6
c               write(*,*) vdet,c1,c2
            ELSE                        ! output semi-infinite case
                EMKT = ZERO
                C1 = V3/V1
                C2 = ZERO
            ENDIF
C thru ALPHA, BETA, V3 and V6, all the RI include a COSI factor in every term!
        RI(1,1) = C1+C2-ALPHA                     !  I0_top     eq. 12a@tau=0
        RI(2,1) = P*(C1-C2)-BETA                  !  I1_top     eq. 12b@tau=0
        RI(1,2) = C1*EMKT + C2*EPKT - ALPHA*EMTC  !  I0_bot     eq. 12a
        RI(2,2) = P*(C1*EMKT-C2*EPKT) - BETA*EMTC !  I1_bot     eq. 12b

        BOND = PI*(RI(1,1)-TT*RI(2,1))/COSI             ! eq. 8 & 11
        COLL = EMTC     ! direct beam per its cross-section
        RETURN
        END
