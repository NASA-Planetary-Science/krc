      REAL FUNCTION SEASTAU (LSUB)
C_Titl  SEASTAU:  seasonally variable atmosphereic  tau
      IMPLICIT  NONE
      INTEGER MROW,MSKIP
      PARAMETER (MROW=362)      ! max number of table entries
      PARAMETER (MSKIP=10)      ! max number of lines to skip through c_end
C_Vars
      INCLUDE 'unic8m.f'
      INCLUDE 'filc8m.f'

C_Args
      REAL LSUB               !in. season; l-sub-s in degrees.
C  If large negative, will read file and save values
C_Desc
C initally reads an text table of two colums; l-sub-s and tau
C for each positive date request . does linear interpolation, with wrap around.
C output will be negative if error occured
C_Hist  Hugh_Kieffer  2006sep09
C 2011jul31 HK  Use  FINTERP rather than  RNDEX (which was failing) and RNTERP
C 2012feb26 HK  Remove unused variables
C 2016may12 HK Update include names
C_End6789012345678901234567890123456789012345678901234567890123456789012_4567890

C local variables
      REAL XXX(MROW)            ! holds the table of  Ls in degrees
      REAL YYY(MROW)            ! holds tau on that date
      INTEGER I,KK              ! number of defined dates
      REAL G,OUT
      REAL FINTERP              ! called function
      INTEGER READTXT360        ! called function
      SAVE XXX,YYY,KK

D      IF (IDB5.GT.3) WRITE(*,*)'SEASTAU',LSUB,kk,yyy(1),yyy(kk) ! <<<<< temporary
      OUT=-1.                   ! possible error flag

      IF (LSUB .LT. -90.)  THEN ! read the file
         KK=READTXT360(FVTAU,IOD3,XXX,YYY)
         IF (IDB5.NE.0) THEN
            WRITE (*,*) 'SEASTAU FVTAU=', FVTAU
            WRITE (*,*) 'SEASTAU kk=', KK
            DO I=1,KK
               WRITE (*,*) I,XXX(I),YYY(I)
            ENDDO
         ENDIF
        OUT=KK
        IF (KK.LT. 1)
     +          WRITE(IOERR,*)'SEASTAU error opening input file =',FVTAU

        ELSE                    ! interpolate

          G=AMOD(LSUB,360.)     ! insure within 0. to 360.
          OUT=FINTERP(G,XXX,KK,YYY)   ! linear interpolation 
          IF (IDB5.GT.1) WRITE(*,*)'SEASTAU',LSUB,G,OUT
      ENDIF

      SEASTAU=OUT
D     WRITE (*,*) 'SEASTAU lsub,out=', LSUB,G,F,OUT
      RETURN
      END
