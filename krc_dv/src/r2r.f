      SUBROUTINE R2R(RA,RB,N)
C_TITL  R2R   Real-to-real array move or fill
C_ARGS
      REAL RA(1)  ! [In]  Source array or constant
      REAL RB(1)  ! [Out] Destination array
      INTEGER N   ! [In]  Number of items to move:
C                      If  N>0, copies the source array   
C                      If  N<0, replicates the first item in  RA
C                      Into  RB  |N| times.  I.e., fill  RB
C                       with a constant value.
C_DESC  Note:  Can be used to move single value or to move
C       binary words between variables of different type.       
C       Macro version checks absolute location of arrays and
C       goes forward or backward through arrays as required
C       to insure source values are not overwritten before
C       being read.
C       *****  fortran version does not have overlap protection  *****
C_HIST  70--  Hugh_H_Kieffer at  UCLA
C       78--  Eric_Eliason  Macro version into system library
C       81Oct18  HHK  Fortran version
C 2016may22 HK  Untabify, replace computed goto, consider overlap
C       _END
        INTEGER III,JA,JB
        REAL RA1
        IF (N .LT. 0) THEN  ! replicate constant
          III=-N
          RA1=RA(1)
          DO I=1,III
            RB(I)=RA1
          ENDDO
        ELSEIF (N .GT. 0) THEN  ! copy
          III=N
          JA=LOC(RA)            ! Location in memory of source
          JB=LOC(RB)            ! " " destination
C          print *,'R2R locs:',JA,JB ! testing
          IF ((JB.GT.JA) .AND. (JB.LE.(JA+III))) THEN ! would overwrite
            DO I=III,1,-1      ! do backwards
              RB(I)=RA(I)
            ENDDO
          ELSE                  ! destination later (in time) than source
            DO I=1,III         ! do forwards
              RB(I)=RA(I)
            ENDDO
          ENDIF
        ENDIF
        RETURN
        END
