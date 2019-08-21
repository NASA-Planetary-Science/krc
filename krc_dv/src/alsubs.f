      REAL FUNCTION ALSUBS (KODE, ARG, MMY)
C_Titl  ALSUBS  Convert L-sub-s  <-> MJD
      IMPLICIT NONE
      INTEGER*4 KODE              ! IN. Controls which direction to do convertion
C    1:  Convert from MJD  to L_s
C    2:  Convert L_s to  MJD
      REAL*4 ARG                  ! IN. Days into year (kode=1) or Ls (kode=2)
      INTEGER*4 MMY               ! IN. Modern Mars Year 
C Uses cosine series analytic approximation. Error < .01 degree L_s. ~ 1990.
C_Hist 2002mar07  Hugh_Kieffer Adopted from l_sub_s.pro
C 2013jun09 HK Use fits from qlsam.pro to MJD rather than days from MY start
C            Mean absolute error about .025 degree or .045 days from Allison00
C 2014feb26 HK Replace all DOUBLE PRECISION with REAL*8. Explicit kind for arguments
C_End6789012345678901234567890123456789012345678901234567890123456789012_4567890

      REAL*8 F,X,AA(8),BB(8)
      REAL*8 MYEAR /686.97124D0/ ! mean Mars tropical year
      REAL*8 MJD0 /151.28423D0/ ! MJD of Ls=0 near J2000.0. Start MY25

      DATA AA /-10.327394D0,57.293061D0,10.690779D0,-0.15156414D0
     &     ,-0.62572785D0,1.2689212D0,-0.05001041D0,-0.41539527D0/

      DATA BB /19.722797D0,109.33485D0,-20.42314D0,-0.33143763D0
     &     ,-0.71602485D0,0.90737588D0,0.02920373D0,-0.99918222D0/

      IF (KODE.EQ.1) THEN       ! from MJD to L_s
         X=DBLE(ARG-MJD0)/MYEAR ! fractional martian year
C         X=MOD(X,1.D0)          ! "
        X=6.2831853072D0*X      ! radians from start of martian year  
        F= AA(1) +AA(2)*X +AA(3)*COS(X-AA(4))
     & + AA(5)*COS(2.D0*X-AA(6)) +AA(7)*COS(3.D0*X -AA(8))
      ELSE             ! from degrees L_s to days since start of MY 25
        X=0.017453292520D0*ARG  ! convert degrees to radians 
        F= BB(1) +BB(2)*X +BB(3)*COS(X-BB(4))
     & + BB(5)*COS(2.D0*X-BB(6)) +BB(7)*COS(3.D0*X -BB(8))

        F=F+(MMY-25)*MYEAR

      ENDIF

      ALSUBS=REAL(F) ! convert back to single precision
      RETURN
      END
