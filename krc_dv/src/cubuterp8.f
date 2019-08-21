      SUBROUTINE CUBUTERP8 (KODE,NIN ,TENS,YY, ZZ)
C_Titl  CUBUTERP8  Cubic interpolation of uniformly spaced points to higher dens.
      IMPLICIT NONE
C_args
      INTEGER*4 KODE ! in. Control: 1= setup,  2=process YY,  +2 for debug print
                     !  +4 for detail of each point
      INTEGER*4 NIN  ! in. Dual use number: When KODE is:
C                    odd: KIM=Number of output points per input point: 2 to 32
C                   even: NY=Number of points in yy, ignoring the wrap extensions
      REAL*8 TENS    ! in. Spline tension parameter, zero to 1., normally 0.5
C                         Ignored when KODE is odd.
      REAL*8 YY(*)   ! in. Y values, uniformly spaced in x, augmented by two 
                     !   leading  and one following, typically either wrapped. 
                     ! or constant. Ignored when KODE is odd.
      REAL*8 ZZ(*)   ! out. (KIM*NY) Interpolated y values; when KODE is even
C_Calls:  SIGMA8
C_Desc
C Assumes that the last output point is to be coregistered with the
C last original input point. Thus, the first output point is 1/kim into 
C the  interval between the low-wrap point and the first original point;
C this follows the KRC hour convention 
C Follows http://codeplea.com/introduction-to-splines for cubic Hermite splines
C Algorithm was first tested in IDL: q.pro @ 65 calls cubuterp
C MUST call with kode odd first, and not modify hhh before [multiple] calls 
C with kode even and yy of the same size but different values.
C_Liens
C  BEWARE. Not usable by more than one caller at a time.
C_Hist  2016may16  Hugh Kieffer
C 2016jul05 HK Convert from CASE to IF; allow debug print.
C 2016aug19 HK Fix by IDL tests. Add report of mean and Standard Deviation
C_End

      INTEGER*4 I,I0,K,KIM,KIT,NY,NZ
      REAL*8 Y1,Y2,RM1,RM2, RIM,T1,T2,T3
      INTEGER*4 MMAX
      PARAMETER (MMAX=32)
      REAL*8 HH1(MMAX),HH2(MMAX),HH3(MMAX),HH4(MMAX) ! Hermite basis functions
      SAVE HH1,HH2,HH3,HH4,KIM ! Makes routine not shareable
C   
      IF (KODE .GT. 2) THEN ! DEBUG
        WRITE(*,*) 'CUBUTERP: KODE,NY,TENS',KODE,NY,TENS
      ENDIF

      IF (MOD(KODE,2).EQ.1) THEN !---SETUP THE HERMITE BASIS FUNCTIONS ----
        KIM=NIN                 !  transfer to a saved variable
        IF (KIM .GT. MMAX) THEN
C          CALL BACKTRACE
          PRINT *,'CUBUTERP: NZ/NY TOO LARGE',KIM,MMAX
          KIM=MMAX
        ENDIF
        RIM=DBLE(KIM)            ! " AS REAL
        KIT=KIM-1             ! number of new interpolation points per interval
        DO I=1,KIT            ! no need for interval end points
          T1=DBLE(I)/RIM        ! fractional way into the interval
C          TT(I)=T1
          T2=T1**2              ! T^2
          T3=T1*T2              ! T^3
          HH1(I)= 2.*T3-3.*T2+1. !  H_1
          HH2(I)=-2.*T3+3.*T2
          HH3(I)=T3-2.*T2+T1
          HH4(I)=T3-T2          ! H_4
C          IF (I.EQ.3) WRITE(*,33)'3 KIM,T*',KIM,T1,T2,T3
        ENDDO

        IF (KODE .GT. 2) THEN      ! DEBUG
C          WRITE(*,*)'  YY=',SIZE(YY)
C          WRITE(*,*)'  ZZ=',SIZE(ZZ)
 33       FORMAT(A,((10F9.3)) )
          WRITE(*,*) 'CUBUTERP: Hermite basis functions'
          WRITE(*,33) 'HH1',(HH1(I),I=1,KIT)
          WRITE(*,33) 'HH2',(HH2(I),I=1,KIT)
          WRITE(*,33) 'HH3',(HH3(I),I=1,KIT)
          WRITE(*,33) 'HH4',(HH4(I),I=1,KIT)
        ENDIF

      ELSE  !---------------------- Process --------------------------------

C Interpolate between relative points y2 and y3
C Would like to use intrinsic MATMUL function but that requires fixed array size
C     VV=MATMUL(HH,BB) ! values within one input interval
        NY=NIN ! number of original data points
        KIT=KIM-1
        NZ=KIM*NY               ! number of output points
        DO K=1,NY               ! original data points
C     current group of four, Y0,  y1 <active>  y2,  y3
C     Index in augmented     k    k+1         k+2   k+3
C     BB_i are factors for h_i_
          Y1=YY(K+1)           ! y1
          Y2=YY(K+2)           ! y2
          RM1=TENS*(YY(K+2)-YY(K)) !    m1=car*(yy[2]-yy[0]) 0-based
          RM2=TENS*(YY(K+3)-YY(K+1)) !  m2=car*(yy[3]-yy[1]) " 
          I0=(K-1)*KIM           ! base for each output interval
          DO I=1,KIT             ! out=HH_ij ## BB_i
            ZZ(I0+I)= HH1(I)*Y1 +HH2(I)*Y2 +HH3(I)*RM1 +HH4(I)*RM2
          ENDDO
          ZZ(I0+KIM)=Y2 ! copy the upper original point as the knot
          IF (KODE .GT. 4)  WRITE(*,34)  ! debug
     &      'CUBUTERP: K,I0,BBx',K,I0,Y1,Y2,RM1,RM2
        ENDDO
          IF (KODE .GT. 2)  THEN  ! debug
            WRITE(*,34)'CUBUTERP: K,I0,y,m',K,I0,Y1,Y2,RM1,RM2
 34         FORMAT(A,2I6,((10F9.3)) )
            CALL SIGMA8 (YY(3),NY, Y1,Y2)   ! last 2 args are mean and std_dev
            CALL SIGMA8 (ZZ   ,NZ, RM1,RM2) !  " " "
            PRINT *,'CUBUTERP: Stats: input, output',Y1,Y2,RM1,RM2 
C mean and stdDev of output should be close to those of input.
          ENDIF
      ENDIF
      
      RETURN
      END
