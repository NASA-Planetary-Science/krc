      SUBROUTINE TYEARP (IQ,IR)
C_Titl  TYEARP  Store seasonal midnight T; make annual forecast for deep layers
C_Vars
	INCLUDE 'krccom.inc'
	INCLUDE 'latcom.inc'
	INCLUDE 'hatcom.inc'
        REAL  PCOM1(22),SLP,P24,PHFXX(3),PCOM2(33)
	COMMON /PORBCM/ PCOM1,SLP,P24,PHFXX,PCOM2
C the above for  PHFXX rather than  INCLUDE '/home/hkieffer/krc/porb/porbcm.inc'
C  for simplicity
C_Args
	INTEGER IQ      ! in. 1=store   2=forecast
	INTEGER IR	! out. return code,  0=OK -=error +=did nothing        
C_Desc
C Will store only the seasons that are (nearest to) integral years before 
C the forecast date. Need 3 dates [years] to make asymptotic forecast. 
C Storage allows for up to MAXN6
C Forecast algorithm extracted from epred.f
C_Liens  THIS VERSION IS INCOMPLETE AND UNTESTED
C        BEST TO ENSURE THAT   IDISK2  IS NOT POSITIVE
C_Hist 2009mar17 Hugh Kieffer Initial version 
C_End6789012345678901234567890123456789012345678901234567890123456789012_4567890

        REAL YY(3)            ! to hold latest 3 years for one layer-latitude
        REAL D,DL,R,RES,SPY,X,YEAR,YP
        INTEGER IR,J,K,KYEAR

        YEAR=PCOM1(9)           ! length of year
        SPY=YEAR/DELJUL         ! seasons per year
        KYEAR=NINT(SPY)         ! nearest integer to seasons/year 
        IR=KYEAR                ! possible return code
        IF (IDISK2.LT.KYEAR) GOTO 9 ! do nothing
        IR=0                    ! set return code to normal

        IF (MOD(IDISK2-J5,KYEAR).EQ.0) THEN ! store annual result
           K=J5/KYEAR+1         ! annual storage index
           IF(K.GT.MAXN6) GOTO 82 ! too many years
           DO J=1,N4		! all latitudes
              DO I=1,N1PIB		! all layers
                 TMN4Y(K,I,J)=TMN4(I,J) ! save midnight Temperatures
              ENDDO
           ENDDO
           WRITE(22,*)'J5,K,N4=',J5,K,N4 !<<< debug
        ENDIF

	IF (J5.EQ.IDISK2) THEN ! reset layers to forecast annual average
           IF (J5.LT. 2*KYEAR+1 .OR. K.LT.3) GOTO 81 ! <3 years
           RES=ABS(SPY/KYEAR-1.) ! fractional year residual
           IF (RES.GT. 0.01) WRITE(IOERR,*)'TYEAR Year residual= ',res 
           X=5.                  ! years to forecast

           WRITE(22,*)'LatLay    1____years_____3     Pred' !<<< debug
           DO J=1,N4		! all latitudes
              DO I=1,N1PIB      ! all layers
                 CALL R2R(TMN4Y(K-2,I,J),YY,3) ! latest 3 years
                 DL = YY(2)-YY(1) ! slope between first 2 points
                 IF (DL.EQ.0.) GOTO 9 ! no change between first two points
                 YP=YY(3)       ! latest point
                 D = YP-YY(2)   ! latest slope
                 R = D/DL       ! ratio of successive changes
                 IF (R.GT.0.0 .AND. R.LT.1.0) THEN ! asymptotic form
                    YP = YP+ (D/(1./R -1.) )*(1. -R**X) 
                 ELSE           ! linear form
                    YP = YP+ X*D
                 ENDIF 
                 TMN4(I,J)=YP   ! transfer to start for next season
                 WRITE(22,33),J,I,YY,YP !<<< debug
 33              FORMAT(2I3,3F7.2,F9.2)
              ENDDO
           ENDDO
        ENDIF
        GOTO 9

 82     IR=IR-1                 ! IR=-2: too many years before forecast
 81     IR=IR-1                 ! IR=-1: less than 3 years saved before forecast
 9      RETURN
        END
