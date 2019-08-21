        SUBROUTINE CON4 (KODE,XA,YA,NA,XB,NB,SIZE, YB,IER)
C_Titl  CON4 convolution with box, triangular, gaussian or custom filter
C_Args  i=in  o=out  b=both
        INTEGER KODE    !i convolution filter code, sign sets point vrs integral
C 1=box, 2=triangular, 3=gaussian, 4=custom, 
C 0=define custom filter shape by the next 3 arguments.
C + means piecewise linear integration.   - means evaluate only at  XA points
C + is robust to sparse or variable spacing in  A.
C - is much faster, and usefull if  A is dense relative to  FWHM.
        REAL XA(*)      !i wave-points of original spectrum. must be monotonic.
        REAL YA(*)      !i values of original spectrum (e.g., transmission).
        INTEGER NA      !i number of points in original spectrum.
C for  KODE=0, above 3 are for the filter, and remaining arguments are ignored.
        REAL XB(*)      !i wave-points of output spectrum. can be in any order.
        INTEGER NB      !i number of points in output spectrum.
        REAL SIZE(2)    !i box & triang: (1) =  FWHM, full-width at
C               .  half-maximum) of filter; not used for custom.
C               .  GAUSSIAN, (1)=sigma, (2)=wing extent in sigma.
        REAL YB(*)      !o values of output spectrum
        INTEGER IER     !o error return. 0=normal
C               + = number of points which are outside the input spectral range
C               -1 = no output points requested
C               -2 = too few input points
C               -3 = invalid control code  KODE
C               -4 = filter width is not positive
C               -5 = no custom filter stored;  KODE=4
C               -6 = invalid # points for filter;  KODE=0
C_Desc
C  Terminology here is in terms of spectra, but routine is basically  Y(x).
C the input spectrum can be irregularly spaced, but must be monotonic (either
C ascending or descending).
C  Will properly weight every point within the filter; any part of filter which
C       falls outside of input spectrum is ignored.
C  Independant variable [e.g., wavelength] and  SIZE(1) must be in same units.
C  Output points which fall outside input spectrum range are set to value of
C       nearest point.
C  To simply filter an array, use same array for  XA and  XB.
C .  Nature of x array for the convolution function:
C box; implicit, need only sum  YA over proper interval
C triangular or gaussian; evaluate at each  XA point
C custom; have explicit, progress through union of  XA & filter points.
C ==== two methods of summation for each type of filter:
C   when  KODE<0, evaluates the filter function only at spectral  (_A) points
C that are interior to the filter; if filter entirely between points, then
C uses linear interpolation.
C   when  KODE>0, computes exact area under the set of points connected
C by straight lines, even if none fall within the filter interval. For gaussian,
C approximates the defined wing by 10 (hard-coded) points (may have abrupt edge)
C  KODE=0: input  XA = offset (+&-) from filter center, must be increasing
C  YA = filter value, need not be normalized, but points should not be of 
c opposite sign. first and last point should be zero to avoid sharp edges 
C if piecewise linear integration will be requested.
C
C  Custom filter does not need to be symmetric, and may be biased to either side
C   of zero to any extent.
C  Calling for triangular or gaussian integration  (KODE = 2,3) will overwrite
C   the stored custom filter.
C
C  Note: the returned value  (YB) for a point will be set to .777 , and a 
C   message printed to unit IEL, if logic here fails to find valid input 
C   points.  This did not happen in any testing.
C_FILE
        PARAMETER (IEL=7)       ! error log unit
C_Lim
        PARAMETER (MAXC=401)     !maximum # points in custom or gauss filter.
C_Call  RNDEX  RTERP
C_Hist  91dec23  Hugh_H_Kieffer  USGS_Flagstaff   original version
C 96jan24 add printout for point sums to debug flase problem. all is OK in this routine
C 97nov18  HHK increase size of  MAXC from 151 to 401 (for  ROLO  SWIR bands)
C 2014apr28 HK Untabify
C_End
        REAL*4 XC(MAXC),YC(MAXC)        ! holds filter shape
        LOGICAL ASCEND                  !  XA increases with index
        LOGICAL MOREA                   ! more  A points are available
        INTEGER NCUST /0/               ! size of custom filter stored

C  _A is the original spectrum, x values must be monotonic, of either sign
C  _B are the output points, x values in any order
C  _C is the filter, x values must be increasing
C for  KODE +, filter result is  INTEGRAL(AC) dx /  INTEGRAL(C) dx.
C do as the sum of integration over each interval of the product of two linear
C functions. interval is between points in the union of  A &  C.  A1 &  A2
C represent value of piecewise-linear  A function at endpoints of interval;
C   same for  C.
C =  SUM  (2(A1C1+A2C2)+A1C2+A2C1) del_x / sum  (C1+C2) del_x
C go through  A array so that x is always increasing!

        IER = 0
D       WRITE (IEL,*)' CALLED WITH KODE = ',KODE

        IF (KODE.EQ.0) THEN                     ! save custom filter
          IF (NA.LT.3 .OR. NA.GT.MAXC) GOTO 86  ! invalid number of points
          NC = NA
          NCUST = NC    ! set flag that says custom filter is stored
          DO I=1,NC
            XC(I) = XA(I)
            YC(I) = YA(I)
            ENDDO
          GOTO 9                ! return
        ENDIF

C formal error tests
        IF (NB.LT.1) GOTO 81                    ! no output points
        IF (NA.LT.2) GOTO 82                    ! too few input points

C get the offsets to the edges of the filter
        FWHM = SIZE(1)
        GOTO (11,12,13,14,83), IABS(KODE)
11      WING1 = FWHM/2.         ! box
        GOTO 135
12      WING1 = FWHM            ! triangular
        GOTO 135
13      WING1 = SIZE(1)*SIZE(2) ! gaussian
135     IF (FWHM.LE.0.) GOTO 84         ! invalid convolution width
        WING2 = WING1
        GOTO 20
14      WING1 = -XC(1)          ! custom
        WING2 = XC(NC)
20      ASCEND = XA(NA).GT.XA(1)
        IF (ASCEND) THEN        ! monotonic increasing
          I3 = 1                ! set increment for going thru  XA
          IALOW = 1             ! index of low end of  XA
          IAHIGH = NA           ! index of high end of  XA
          JLOW = 0              !  LOCATE return if point off low end of  XA
          JHIGH = NA            !   or off high end of  XA
        ELSE                    ! monotonic decreasing
          I3 = -1
          IALOW = NA
          IAHIGH = 1
          JLOW = NA+1
          JHIGH = 1
        ENDIF

C common case expected to be that most  XB points, and hence the filter
C will be interior to  XA, and that filter will be wide enough that several
C  XA points fall within it. code for clarity of this condition, and do not
C worry about extra execution time for the other special cases.

C for point evaluation  (KODE -), loop runs over all  A points within
C (inclusive of endpoints) filter. develop logic that finds these.
C for integration  (KODE +), "loop" includes all  A intervals that overlap
C filter, so first point beyond filter is needed (unless filter end is exactly
C on a point in  A); use additional tests for these.

      IF (KODE.LT.0) THEN       !======================= point evaluation

        DO K=1,NB                       ! do each output point
D         KK=0
          X = XB(K)-WING1               ! low end of filter
          IF (X.GE.XA(IAHIGH)) THEN     ! at or off high end of  A
            YB(K) = YA(IAHIGH)          !   set result to nearest point
            IF (X.GT.XA(IAHIGH)) IER = IER+1 !   up the out-of-bounds count
          ELSE
            KASE = 1
            GOTO 500            ! transfer to  "LOCATE" and come back here
501         IF (JL.EQ.JLOW) THEN        ! before low end of  XA
                I1 = IALOW      ! try for some points 
            ELSE
                I1 = JL         ! first point is that returned by  LOCATE
                IF (X.GT.XA(I1)) I1=I1+I3       ! only if precisely coincident.
            ENDIF
D           write(22,*)'JL,I1,IAHIGH,I3=',JL,I1,iahigh,I3
C  I1 now set at first point in  A at or above low end of filter

            SUMY = 0.                   ! initialize sums
            SUMW = 0.
D           write(22,*)'Defined XA'
D           write(22,'(10f10.4)') (xa(i),i=1,na)
D           write(22,*)'Defined YA'
D           write(22,'(10f10.4)') (Ya(i),i=1,na)
D           write(22,*)'Defined filter X'
D           write(22,'(10f10.4)') (xc(i),i=1,nc)
D           write(22,*)'Defined filter y'
D           write(22,'(10f10.4)') (yc(i),i=1,nc)
            DO IA=I1,IAHIGH,I3  ! do each point within filter
              XR = XA(IA)-XB(K)         ! position relative to filter
              IF (XR.GT.WING2) GOTO 180 ! outside of filter
              IF (KODE.EQ.-1) THEN      ! box filter
                W = 1.
              ELSEIF (KODE.EQ.-2) THEN  ! triangular filter
                W = 1. - ABS(XR/FWHM)
              ELSEIF (KODE.EQ.-3) THEN  ! gaussian filter
                W = EXP(-0.5*(ABS(XR)/FWHM)**2)
              ELSEIF (KODE.EQ.-4) THEN  ! custom filter
                Q=RNDEX (XR,XC,NC)      ! guarenteed to be within filter
                W=RTERP (YC,Q)          !  RNDEX & RTERP handle limiting cases
D               write(22,*)'IA,XR,YA,Q,W=', IA,XR,YA(ia),Q,W
              ENDIF
D       KK = KK+1
              SUMY = SUMY + W*YA(IA)
              SUMW = SUMW + W
              ENDDO
180         IF (SUMW.NE.0.) THEN
                YB(K) = SUMY/SUMW
            ELSE        ! filter overlaps no element of  A
              IF (JL.EQ.JLOW) THEN      ! filter entirely below  A
                YB(K) = YA(IALOW)       ! use end-point value
                IER = IER+1
              ELSE              ! filter must have fallen within gap in  A
                I1 = JL+I3              ! interpolate to filter center
        YB(K) = YA(JL) + (XB(K)-XA(JL))*(YA(I1)-YA(JL))/(XA(I1)-XA(JL))
              ENDIF
            ENDIF
          ENDIF
D       WRITE(IEL,*) 'K+',K,KK,SUMW,SUMY
          ENDDO

        ELSE                    !========================= integration

C triangular and gaussian use the same logic as custom filter
        IF (KODE.EQ.2) THEN     ! setup triangular
          NCUST = 0
          NC = 3
          XC(1) = -FWHM
          YC(1) = 0.
          XC(2) = 0.
          YC(2) = 1.
          XC(3) = FWHM
          YC(3) = 0.
        ELSEIF (KODE.EQ.3) THEN ! setup gaussian
          NCUST = 0.
          IG = (MAXC-1)/2       ! # points on wing, max allowed =  (MAXC-1)/2
          NC = 2*IG+1                   ! # total points
          DELSIG = SIZE(2)/FLOAT(IG)    ! spacing in sigma
          DELX = SIZE(1)*DELSIG         ! spacing in x
          DO I=1,NC                     ! fill filter arrays
            P = I-IG-1                  ! integer offset from midpoint
            XC(I) =P*DELX
            YC(I) = EXP(-0.5*(P*DELSIG)**2)
D       WRITE (IEL,*) I,XC(I),YC(I)
            ENDDO
          WING1 = -XC(1)        ! do again to avoid roundoff problems
          WING2 = XC(NC)
        ELSEIF (KODE.EQ.4) THEN
          IF (NCUST.EQ.0) GOTO 85       ! no custom stored
        ENDIF

C proceed by doing succesive "intervals" in the union of points in the original
C spectrum and the filter; both the original spectrum and the filter are thus
C linear across an interval.
C  XR = position of a point relative to center of filter
C  X1 = position of lower end of current interval relative to center of filter
C  A1 =  Y value of  A array (specturm)       at lower end of current interval
C  C1 =  Y value of  C array (filter)         at lower end of current interval
C  I1 = index in  A array (spectrum) at or above upper end of current interval
C  IC = index in  C array (filter)   at or above upper end of current interval
C  X2,A2,C2 are values at upper end of current interval

        DO K=1,NB                       ! do each output point
D         KK = 0
          X = XB(K)-WING1               ! low end of filter
          IF (X.GE.XA(IAHIGH)) THEN     ! at or off high end of  A
            YB(K) = YA(IAHIGH)          !   set result to nearest point
            IF (X.GT.XA(IAHIGH)) IER = IER+1 !   up the out-of-bounds count
            GOTO 300
          ENDIF
          KASE = 2              ! transfer to  "LOCATE" and come back here
          GOTO 500              ! see  LOCATE for definition of  JL returned.   
502       IF (JL.EQ.JLOW) THEN  ! before low end of  XA
              IF ((XB(K)+WING2).LE.XA(IALOW)) THEN  ! filter entirely below  A
                IER = IER+1
                YB(K) = YA(IALOW)
                GOTO 300
              ELSE              ! should get some points
                A1 = YA(IALOW)
                X1 = XA(IALOW)
                I1 = IALOW+I3
              ENDIF
          ELSE  ! interior to  A, need values at low end of filter
            X1 = X
            I1 = JL+I3
            A1 = YA(JL) + (X1-XA(JL))*(YA(I1)-YA(JL))/(XA(I1)-XA(JL))
          ENDIF
C now have  X1&A1 at lower edge of filter;  I1 set to upper end of this interval

          SUMY = 0.                     ! initialize sums
          SUMW = 0.
        IF (KODE.EQ.1) THEN     !-------------------  BOX  FILTER

            DO IA=I1,IAHIGH,I3                  ! do each point within filter
              XR = XA(IA)-XB(K)         ! position relative to filter
              IF (XR.GT.WING2) GOTO 280 ! outside of filter
                DX = XA(IA)-X1
                SUMY = SUMY + DX*0.5*(YA(IA)+A1)        ! trapazoid sum
                SUMW = SUMW + DX
D       KK = KK+1
                A1 = YA(IA)             ! reset lower point
                X1 = XA(IA)
                ENDDO
            GOTO 285

280         IL = IA-I3          ! point was outside; back up to prior interval
            X2 = XB(K)+WING2    ! this logic should work even if no points fall
            DX = X2 - X1        !   within the filter.
            A2 = YA(IL)+(X2-XA(IL))* (YA(IA)-YA(IL))/(XA(IA)-XA(IL))
            SUMY = SUMY+DX*0.5*(A2+A1)  ! last fractional step
            SUMW = SUMW+DX
        ELSE                    !-------------------  WEIGHTED  FILTER

C basic loop for two piecewise linear functions;
C   start at minimum  X, proceed taking next point
C in  XA or  XB+XC until reach the end of  XC array.
C this code reevaluates the slope of  C function at each  A point. this could be
C avoided by compiling  C slope at the start and at each new  C point. this
C would require slope code in two places, or, going through the 244 loop one
C extra time with  X2=X1 the first time.

          IA = I1               ! next point in  XA
          IL = I1-I3            ! prior point
          MOREA=.TRUE.
C value of  C at low end of first interval
        IF (JL.EQ.JLOW) THEN    ! low end of filter is outside  A
C because low end of filter is before lowest  A point, cannot get  FC1=-1
          FC1 = RNDEX (XA(1)-XB(K),XC,NC) ! find loc of end of  A in filter
          C1 = RTERP (YC,FC1)   ! get value of filter at this point
          IC = FC1+1.           ! set next filter point.
D         WRITE(IEL,*) ' FC1,C1,IC=',FC1,C1,IC  !  FC1 should never be negative
        ELSE
          C1 = YC(1)
          IC = 2        ! next filter point.
        ENDIF
C now have  IA &  IC set to next values to be used in  A &  C arrays

        XCN = XB(K)+XC(IC)      ! location of next  XC filter point
C determine whether next point is in  A or  C array.
244     IF (XA(IA).LE.XCN) THEN
          X2 = XA(IA)           ! get values of this point
          A2 = YA(IA)
          IL = IA
          IF (IA.EQ.IAHIGH) MOREA=.FALSE.
          IA = IA+I3            ! and point to next point
          IF (XCN.EQ.X2) GOTO 377 ! rare case of  XA &  XC coincide
          XC2 = X2-XB(K)
        C2 =YC(IC-1)+(XC2-XC(IC-1))*(YC(IC)-YC(IC-1))/(XC(IC)-XC(IC-1))
        ELSE
          X2 = XCN
          DX = XA(IA)-XA(IL)
D         IF (DX.LE.0.) THEN
D           WRITE (IEL,'(A,4I4,F7.4)') 'KODE,K ...',KODE,K,KK,IA,IL,X2
D           DX=1.
D         ENDIF
          A2 = YA(IL)+(X2-XA(IL))* (YA(IA)-YA(IL))/DX
377       C2 = YC(IC)
          IC = IC+1   ! increment  IC even though it may go beyond defined array
          XCN = XB(K)+XC(IC)
        ENDIF
        DX = X2-X1
        SUMY = SUMY + DX*(2.0*(A1*C1+A2*C2) + A1*C2 + A2*C1)
        SUMW = SUMW + DX*3.0*(C1+C2)
D       KK = KK +1
        X1 = X2         ! reset lower boundary
        A1 = A2
        C1 = C2
        IF (IC.LE.NC .AND. MOREA) GOTO 244 ! loop until last filter point used

        ENDIF                   !-------------------  END  FILTER  TYPE
285     IF (SUMW.NE.0.) THEN
                YB(K) = SUMY/SUMW
        ELSE            ! (this should never occur with valid input arrays)
                YB(K)=0.777
D               WRITE (IEL,*) 'SUMW=0 @ KODE,K,X,JL', KODE,K,X,JL
D               WRITE (IEL,*) 'I1,IA,IC,C1,C2', I1,IA,IC,C1,C2
        ENDIF
300     CONTINUE
D       WRITE(IEL,*) 'K+',K,KK,SUMW,SUMY
        ENDDO

        ENDIF                   ! ===================  POINT  TREATMENT

9       CONTINUE
D       WRITE (IEL,'(1X,10F7.3)') (YB(K),J=1,NB)
        RETURN
86      IER = IER-1     ! -6 = invalid # points for filter;  KODE=0
85      IER = IER-1     ! -5 = no custom filter stored;  KODE=4
84      IER = IER-1     ! -4 = filter width is not positive
83      IER = IER-1     ! -3 = invalid  KODE
82      IER = IER-1     ! -2 = too few input points
81      IER = IER-1     ! -1 = no output points requested
        GOTO 9

C=============  LOCATE  SUBROUTINE  ========================
C  LOCATE locate interval in table containing target value by repeated bisection
C-desc similiar to  NUM_REC routine, but with pre-check on ascending.
C hardcode here to use the  XA array, looking for location of  X.
C because interest here is in first  XA in filter,
C modify tests to be closed at lower-value end of  XA interval, open above;
C find interval such that  X is in  [JL,JL+I3) interval; 
C   where  I3 is =1 for ascending, -1 for descending.
C result is in  JL:     ascending       descending      preset tests
C  X below table          0               NA+1            JLOW
C  X above table          NA              1               JHIGH
C-end
C for descending, reverse the use of indexes; x test is the same as ascending;
C   each change is still to move limits toward the requested x value

500     IF (ASCEND) THEN	! ascending  XA

	  JL=0
	  JU=NA+1
 510	  IF (JU-JL.GT.1) THEN
	    JM=(JU+JL)/2
	    IF (X.GE.XA(JM)) THEN ! make test closed at low index end
	      JL=JM
	    ELSE
	      JU=JM
	    ENDIF
	    GO TO 510
	  ENDIF
	  
        ELSE			! descending  XA

	  JL = NA+1
	  JU = 0
 520	  IF (JL-JU.GT.1) THEN
	    JM=(JU+JL)/2
	    IF (X.GE.XA(JM)) THEN
	      JL=JM
	    ELSE
	      JU=JM
	    ENDIF
	    GO TO 520
	  ENDIF
	  
        ENDIF			! done one otf the two
D       WRITE (IEL,*) 'JL,X,X(JL),JM',JL,X,XA(JL),JM
        GOTO (501,502),KASE
	
        END
