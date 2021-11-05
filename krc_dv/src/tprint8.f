      SUBROUTINE TPRINT8 (IQIN) 
C_Titl  TPRINT printed output routine
C_Vars
      INCLUDE 'krcc8m.f'
      INCLUDE 'latc8m.f'
      INCLUDE 'dayc8m.f'
      INCLUDE 'unic8m.f'
      INCLUDE 'filc8m.f'
C_Args
      INTEGER*4 IQIN            ! in. what to print: 
C   +=also page title first       calling routine line number as of 2010jan22 
C 1=+ program description OBSOLETE   krc.f:95IF(LP1
C 2=+ all parameters [& depth table]   krc.f:97IF(LP2   tcard.f:283beforeStop
C 3=+ latitude page titles           tlats.f:273
C 4=convergence summary {depth table} tlats.f:343 IF(LP4) tday.f:458afterBlowup  
C 5=+ latitude summary               tseas.f:80
C 6=+ layer min and max temperatures tseas.f:81
C 7=blowup conditions                tday.f:456
C 8=page heading                     tcard.f:129 tlats.f:97 
C 9=one line for "one=point' mode    krc.f:110

C_Calls  none
C_Hist  87sep21 Hugh Kieffer  CHANGE format 142 to  G, and narrow depth table
C       93mar03  ECisneros ported to unix 
C 97feb13  HK major revision
C 97aug22  HK correct printed scales to  MKS
C 2002mar07 HK Add one line output for "one-point" mode
C 2002aug04 HK Fix errors in interpolation for one-point temperatures
C 2005nov19 HK Add print of depth to top of 2nd layer
C 2008nov13 HK Add input card for T-dependent conductivity
C 2009feb24 HK Briefly try using titcom.inc and block data for parameter titles
C               but prints left-adjusted, not useful for aligning input.
C 2009may10 HK add TITONE as one-point comment field
C 2012feb26 HK Remove unused variables
C 2013jan30 HK Print version number
C 2013feb21 HK Remove leading carridge control characters from format statements
C 2014feb13 HK Correct depth table labeling from mass being as layer center
C 2014feb25 HK Set most variables to *4.  Justify
C 2014mar10 HK Make  REAL*8  version    may04, Increase some format precision
C 2014jun02 HK Accomodate I15 being large as a flag for TUN8
C 2015dec24 HK Add print of bottom depth to layer table
C 2016feb-- HK Convert some names to V3.3  Bypass layer table here
C 2018jan30 HK Replace DDT with PhotoF
C 2018jul01 HK Update a few guide lines to v356
C_End6789012345678901234567890123456789012345678901234567890123456789012_4567890
C
      INTEGER*4 I,J,J3P1,K,IQ
      INTEGER*4 IPG /0/
      REAL*8 Q2,Q4,Q6,TOUTO
D        WRITE(IOSP,*)'TPRINT CALLED ',IQIN  !debug

      IQ = IQIN
      GO TO (800,800,800,400,800,800,700,800,900,99), IQ ! print page title
C              1   2   3   4   5   6   7   8   9  +
C
C  PRINT all parameters  (IQ = 2)
C
 200  WRITE (IOSP,210) FINPUT,FOUT,FDISK
 210  FORMAT (' Input file: ',A/' Print file: ',A/' Save file: ',A)

      WRITE (IOSP,212) (FD(I),I=1,NFD)
 212  FORMAT ( '     ALBEDO     EMISS   INERTIA     COND2     DENS2'
     &, '    PERIOD  SpecHeat   DENSITY'/1X,8F10.4
     &/'       CABR       AMW    SatPrA    PTOTAL     FANON      TATM'
     &, '     TDEEP   SpHeat2'/1X,8F10.4
     &/'       TAUD     DUSTA    TAURAT     TWILI   ARC2=G0 ARC3=safe'
     &, '     SLOPE    SLOAZI'/1X,3F10.4,3F10.2,2F10.1 
     &/'     TFROST    CFROST    AFROST     FEMIS       AF1       AF2'
     &, '    FROEXT    SatPrB'/1X,F10.4,F10.1,2F10.4,F10.5,3F10.4
     &/'       RLAY      FLAY     CONVF     DEPTH     DRSET PhotoFunc'
     &, '       GGT     DTMAX'/1X,8F10.4
     &/'       DJUL    DELJUL  SOLARDEC       DAU     LsubS    SOLCON'
     7, '      GRAV    Atm_Cp'/1X,F10.3,2F10.4,F10.5,4F10.4
     8/'     ConUp0    ConUp1    ConUp2    ConUp3    ConLo0    ConLo1'
     &, '    ConLo2    ConLo3'/1X,8F10.4
     8/'     SphUp0    SphUp1    SphUp2    SphUp3    SphLo0    SphLo1'
     &, '    SphLo2    SphLo3'/1X,8F10.4)

      WRITE (IOSP,214) (ID(I),I=1,NID)
 214  FORMAT ( '         N1        N2        N3        N4        N5'
     &, '       N24       IIB       IC2'/1X,8I10
     &/'      NRSET      NMHA      NRUN     JDISK     IDOWN    FlxP14'
     &, ' TUN/Flx15     KPREF'/1X,8I10
     &/'      K4OUT     JBARE     Notif    IDISK2',/1X,4I10)

      WRITE (IOSP,216) (LD(I),I=1,NLD)
 216  FORMAT('     LP1    LP2    LP3    LP4    LP5'
     &,         '    LP6 LPGLOB   LVFA   LVFT  LKofT'/1X,10L7
     &/        '   LPORB   LKEY    LSC  LZONE  LOCAL'
     &,         '   LD16 LPTAVE  Prt78  Prt79  L_ONE'/1X,10L7)
D         WRITE(IOPM,*) 'TPRINT: N4,MAXN4=',n4,MAXN4
      WRITE (IOSP,230) 'Latitudes: ',(ALAT(I),I=1,N4)
      WRITE (IOSP,230) 'Elevations:',(ELEV(I),I=1,N4)
 230  FORMAT (/1X,A/(1X,10F7.2))
      GOTO 99

C-      IQ = 4                    ! temporary, to return from header line
C-      GOTO 800                  ! Print page title, then come back to  250

C
C print layer depth table
C
 250  Q2=DENS*SPHT
      WRITE(IOSP,*) 'OBSOLETE layer table written by TPRINT XXXXXXXXXX'
      WRITE(IOSP,260) COND,Q2,DIFFU,SCALE
 260  FORMAT (' Conductiv.=',1PE10.3,'  Dens*Cp=',E10.3,'  Diffu.='
     &     ,E10.3,'  Scale=',E10.3)
      IF (IC2.GE.3 .AND. IC2.LE.N1-1) then
        Q2=DENS2*SPHT2
        Q4=DSQRT(Q2*COND2)       ! inertia
        Q6=XCEN(IC2-1)+BLAY(IC2-1)/2. ! depth to top of 2nd material
        WRITE(IOSP,262),IC2,Q6,Q4
 262    FORMAT(' Beginning at layer ',I3,'  At ',G12.5
     &,' m.   Inertia=',F10.3)
        Q4=COND2/Q2             ! diffusivity
        Q6=DSQRT(Q4*PERIOD*86400./PIVAL) ! Diurnal scale
        WRITE(IOSP,260) COND2,Q2,Q4,Q6
      ENDIF
      WRITE(IOSP,265)
 265  FORMAT(
     &'         ___THICKNESS____    __CENTER_DEPTH__    Total  Converg.'
     &/' LAYER    scale     meter     scale     meter   kg/m^2  factor')
C       123456789 123456789 123456789 123456789 123456789 123456789 123456789
C2345 789 123456789 123456789 123456789 123456789 123456789 123456789 72________
      Q6=0.
      DO 270 I=1,N1
        Q2 = BLAY(I)/SCALE      ! thickness in units of surface scale
        Q4 = XCEN(I)/SCALE      ! center-depth in units of scale
        IF (I.GT.1 .AND. I.LT.IC2) Q6=Q6+BLAY(I)*DENS
        IF (I.GE.IC2) Q6=Q6+BLAY(I)*DENS2 ! layer-center columnar mass
 270    WRITE(IOSP,280) I,Q2,BLAY(I),Q4,XCEN(I),q6,SCONVG(I)
 280  FORMAT (I5,F10.4,F10.4,F10.4,F10.4,F10.3,F8.3)
      WRITE(IOSP,281) Q4+Q2/2.,XCEN(N1)+BLAY(N1)/2. ! at bottom
 281  FORMAT (' Bottom',T26,F10.4,f10.4)
      WRITE(IOSP,290) (N1K(K),K=1,KKK)
 290  FORMAT (' Lower layer of time doubling: ',15I3)
      GOTO 99
C
C latitude page titles  (IQ = 3)
C
 300  WRITE(IOSP,310) DLAT,SDEC,DAM,TEQUIL
 310  FORMAT ('   LAT=',F6.1,'  SOLAR DEC=',F6.2,'   HALF DAY=',F6.2
     &,' DEGREES   EQUILIBRIUM TEMP=',F7.2/)
      WRITE(IOSP,330) (XCEN(I),I=2,N1M1,NLW),XCEN(N1)
 330  FORMAT ('    HOUR  FROST  X = 0.',(11F8.3))
      GOTO 99
C
C print daily convergence summary  (IQ = 4)
C
 400  WRITE(IOSP,410)TTS4(J4),TTB4(J4),DTM4(J4),J3,FROST4(J4)
 410  FORMAT (' SURF AVE =',F6.1,'  BOTM AVE =',F6.1
     &,'  DELTA T =',F7.4,'  NUM DAYS =',I3,'  FROST =',F9.4)
      WRITE(IOSP,430) (I,I=2,N1M1,NLW),N1
 430  FORMAT (' AT START OF DAY'
     &/' DAY  DTMJ SUR_AVE BOT_AVE     1',13I8)
      J3P1=J3+1
      DO 440 J=1,J3P1
 440    WRITE(IOSP,450) J,DTMJ(J),TTS(J),TTB(J),TT1(1,J)
     &       ,(TT1(I,J),I=2,N1M1,NLW), TT1(N1,J)
 450  FORMAT (1X,I3,F6.2,(14F8.1))
      GOTO 99
C
C print latitude summary  (IQ = 5)
C
 500  WRITE(IOSP,'(A)') ' SURFACE TEMP VS LATITUDE AND LOCAL HOUR ANGLE'
      WRITE(IOSP,'(A,20F6.1)') ' LATITUDE =',(ALAT(I),I=1,N4)
      DO 520 I=1,N24
        IF (MOD(I,6).EQ.1) WRITE(IOSP,'(1X)')
 520    WRITE(IOSP,'(5X,I4,A,20F6.1)') I,'HR',(TSF(I,J),J=1,N4)
      WRITE(IOSP,'(A,20F6.1)') ' SURF AVE =',(TTS4(I),I=1,N4)
      WRITE(IOSP,'(A,20F6.1)') ' BOTM AVE =',(TTB4(I),I=1,N4)
      WRITE(IOSP,'(A,20F6.1)') ' GM FROST =',(FROST4(I),I=1,N4)
      WRITE(IOSP,'(A,20F6.3)') ' ALB/dayF =',(AFRO4(I),I=1,N4)
      WRITE(IOSP,'(A,20F6.1)') ' T. EQUIL =',(TST4(I),I=1,N4)
      WRITE(IOSP,'(A,20F6.2)') ' DELTA T. =',(DTM4(I),I=1,N4)
      WRITE(IOSP,'(A,20I6  )') ' NUM DAYS =',(NDJ4(I),I=1,N4)
      WRITE(IOSP,'(A,20F6.2)') ' TS EPRED =',(TAX(MAXN1,I),I=1,N4)
      WRITE(IOSP,'(A,20F6.2)') ' TB EPRED =',(TIN(MAXN1,I),I=1,N4)
      GOTO 99
C
C print max and min layer temperature summaries  (IQ = 6)
C
 600  WRITE(IOSP,'(A)') '0MAXIMUM TEMPERATURE'
      WRITE(IOSP,'(A,19F6.1)') ' LAYER LAT',(ALAT(I),I=1,N4)
      DO 620 J=1,N1
 620    WRITE(IOSP,'(1X,I5,4X,20F6.1)') J,(TAX(J,I),I=1,N4)
      WRITE(IOSP,'(A)') ' MINIMUM TEMPERATURE'
      WRITE(IOSP,'(A,19F6.1)') ' LAYER LAT', (ALAT(I),I=1,N4)
      DO 640 J=1,N1
 640    WRITE(IOSP,'(1X,I5,4X,20F6.1)') J,(TIN(J,I),I=1,N4)
      GOTO 99
C
C print blowup  (IQ = 7)
C
 700  WRITE(IOSP,710) J2,J3,J4,J5
 710  FORMAT ('  TEMP BLOWING UP. J2,J3,J4,J5=',4I10/)
      WRITE(IOSP,'(1X,10E12.4)') (TTJ(I),I=1,N1PIB)
      GOTO 99
C
C print page heading  (IQ =8)  & for 2,3,5,6
C
 800  IPG=IPG+1
      WRITE(IOSP,810) KITLE,VERSIN,NRUN,NCASE,DAYTIM,IPG
 810  FORMAT (A,/,A12,' RUN-CASE',I3,'-',I2
     &,3X,A,'  PAGE=',I3)
      GOTO (99,200,850,250,850,850,99,99,99) ,IQ
C            1   2   3   4   5   6  7  8  9
C print model constants for  IQ = 3,5,6
C
 850  I=I15
      IF (I15.GT.96) I=77       ! If doing fort.7, print SKYFAC
      WRITE(IOSP,860) ALB,EMIS,SKRC,SDEC,DAU,SUBS,DJU5,J5
     &,FD(I14),FD(I)
 860  FORMAT ('     ALBEDO     EMISS   INERTIA SOLAR DEC'
     &,'       DAU       L_S JULIAN DAY    J5   two from KRCCOM'
     &/1X,3F10.4,F10.2, F10.5,2F10.2,I7,2X,2G10.4)
      GO TO (99,99,300,99,500,600,99,99,99) ,IQ
C            1   2   3  4   5   6  7  8  9
C
C Print one line of "one-point" results
C
C Model should have been run for the precise season and latitude of request.
C Need to interpolate temperatures to the "hour" requested.
 900  Q2=HOURO*DFLOAT(N2)/24.     ! real index of requested time in time steps
      I=INT(Q2)                 ! INTEGER index of prior point
      J=I+1                     ! index of next point
      IF (I.LT.1) I=N2          ! wrap around midnight
      IF (J.GT.N2)J=1           ! wrap around midnight
      TOUTO=TOUT(I)+(Q2-DFLOAT(INT(Q2)))*(TOUT(J)-TOUT(I)) ! linear interp
C planetary temperature available only on the hour, so interpolate Tp-Ts
C  and add this to Ts at the exact requested time
      Q2=HOURO*DFLOAT(N24)/24.  ! real index of requested time in "hours"
      I=INT(Q2)                 ! INTEGER index of prior point
      J=I+1                     ! index of next point
      IF (I.LT.1) I=N24
      IF (J.GT.N24)J=1
      Q4=(TPFH(I)-TSFH(I))      ! planetary-kinetic
      Q4=TOUTO+ Q4+(Q2-DFLOAT(INT(Q2)))*((TPFH(J)-TSFH(J))-Q4)
      WRITE(IOSP,903) SUBS,ALAT(1),HOURO,ELEV(1) 
     &       ,ALB,SKRC,TAUD,SLOPE,SLOAZI,TOUTO,Q4,TITONE
C903  format(' 11',2F6.1,F6.2,F5.1,F5.2,F7.1,F5.2,F5.1,F5.0,2f7.1)  Original
C903  format(' 11',2F6.1,F6.2,F5.1,F5.2,F7.1,F5.2,F5.1,F5.0,2f11.5) Robin F.
 903  FORMAT(' 11',2F6.1,F6.2,F5.1,F5.2,F7.1,F5.2,F5.1,F5.0,2f7.2,A20)

 99   RETURN
C
      END
