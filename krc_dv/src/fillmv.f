C_Titl FILLMV  A bunch of routines to Fill or Move N words Byte Integer, Real
C Includes  FILLB FILLI FILLL FILLR FILLD  
C             MVB   MVI   MVL   MVR   MVD MVDF MVDM  MV21D
C BEWARE: these routines still allow writting outside the input arrays! 
C
C_DESC 
C *****  fortran version does not have overlap protection  *****
C_Hist 2016aug22  Hugh Kieffer.  Create to replace R2R series, which combined
C fill and move, thus could not compile with -Wall and debugger could not
C check overflow. 
C 2018jun23 HK Add 

C------------------------------- BYTE --------------------------

      SUBROUTINE FILLB (RA,RB,N)
C_TITL  FILLB  Byte array fill
      IMPLICIT NONE
C_ARGS
      BYTE RA                   ! [In] Source constant
C     BYTE, INTENT(IN) :: RA
      BYTE RB(N)                ! [Out] Destination array
C     BYTE, INTENT(out) :: RB
      INTEGER N                 ! [In]  Number of words to fill:
C     INTEGER, INTENT(IN) :: N  
C_PAUSE
      INTEGER*4 I
      DO I=1,N
        RB(I)=RA
      ENDDO     
      RETURN
      END

      SUBROUTINE MVB (RA,RB,N)
C_TITL  MVB  Byte-to-byte array move
      IMPLICIT NONE
C_ARGS
      BYTE RA(N)                ! [In] Source array or constant
      BYTE RB(N)                ! [Out] Destination array
      INTEGER N                 ! [In]  Number of items to move:
C_PAUSE
      INTEGER*4 I
      DO I=1,N
        RB(I)=RA(I)
      ENDDO      
      RETURN
      END

C------------------------------- I*2 --------------------------
      SUBROUTINE FILLI (RA,RB,N)
C_TITL  FILLI  I*2 array fill
      IMPLICIT NONE
C_ARGS
      INTEGER*2 RA              ! [In] Source constant
      INTEGER*2 RB(N)           ! [Out] Destination array
      INTEGER N                 ! [In]  Number of words to fill:
C_PAUSE
      INTEGER*4 I
      DO I=1,N
        RB(I)=RA
      ENDDO     
      RETURN
      END

      SUBROUTINE MVI (RA,RB,N)
C_TITL  MVI  I*2 to I*2 array move
      IMPLICIT NONE
C_ARGS
      INTEGER*2 RA(N)           ! [In] Source array or constant
      INTEGER*2 RB(N)           ! [Out] Destination array
      INTEGER N                 ! [In]  Number of items to move:
C_PAUSE
      INTEGER*4 I
      DO I=1,N
        RB(I)=RA(I)
      ENDDO      
      RETURN
      END

C------------------------------- I*4 --------------------------
      SUBROUTINE FILLL (RA,RB,N)
C_TITL  FILLL  I*4 array fill
      IMPLICIT NONE
C_ARGS
      INTEGER*4 RA              ! [In] Source constant
      INTEGER*4 RB(N)           ! [Out] Destination array
      INTEGER N                 ! [In]  Number of words to fill:
C_PAUSE
      INTEGER*4 I
      DO I=1,N
        RB(I)=RA
      ENDDO     
      RETURN
      END

      SUBROUTINE MVL (RA,RB,N)
C_TITL  MVL  I*4 to I*4 array move
      IMPLICIT NONE
C_ARGS
      INTEGER*4 RA(N)           ! [In] Source array or constant
      INTEGER*4 RB(N)           ! [Out] Destination array
      INTEGER N                 ! [In]  Number of items to move:
C_PAUSE
      INTEGER*4 I
      DO I=1,N
        RB(I)=RA(I)
      ENDDO      
      RETURN
      END

C------------------------------- R*4 --------------------------
      SUBROUTINE FILLR (RA,RB,N)
C_TITL  FILLR  REAL*4 array fill
      IMPLICIT NONE
C_ARGS
      REAL*4 RA                 ! [In] Source constant
      REAL*4 RB(N)              ! [Out] Destination array
      INTEGER N                 ! [In]  Number of words to fill:
C_PAUSE
      INTEGER*4 I
      DO I=1,N
        RB(I)=RA
      ENDDO     
      RETURN
      END

      SUBROUTINE MVR (RA,RB,N)
C_TITL  MVR  REAL*4 to REAL*4 array move
      IMPLICIT NONE
C_ARGS
      REAL*4 RA(N)              ! [In] Source array or constant
      REAL*4 RB(N)              ! [Out] Destination array
      INTEGER N                 ! [In]  Number of items to move:
C_PAUSE
      INTEGER*4 I
      DO I=1,N
        RB(I)=RA(I)
      ENDDO      
      RETURN
      END

C------------------------------- R*8 --------------------------

      SUBROUTINE FILLD (RA,RB,N)
C_TITL  FILLD  REAL*8 array fill
      IMPLICIT NONE
C_ARGS
      REAL*8 RA                 ! [In]  Source constant
      REAL*8 RB(N)              ! [Out] Destination array
      INTEGER N                 ! [In]  Number of words to fill:
C_PAUSE
      INTEGER*4 I
      DO I=1,N
        RB(I)=RA
      ENDDO     
      RETURN
      END

      SUBROUTINE MVD (RA,RB,N)
C_TITL  MVD  REAL*8 to REAL*8 array move
      IMPLICIT NONE
C_ARGS
      REAL*8 RA(N)              ! [In]  Source array or constant
      REAL*8 RB(N)              ! [Out] Destination array
      INTEGER N                 ! [In]  Number of items to move:
C_PAUSE
      INTEGER*4 I
      DO I=1,N
        RB(I)=RA(I)
      ENDDO      
      RETURN
      END

      SUBROUTINE MVDF (RA,RB,N, FAC)
C_TITL  MVD  REAL*8 array times factor move
      IMPLICIT NONE
C_ARGS
      REAL*8 RA(N)              ! [In]  Source array or constant
      REAL*8 RB(N)              ! [Out] Destination array
      INTEGER N                 ! [In]  Number of items to move:
      REAL*8 FAC                ! multiply factor
C_DESC  
C      arg2 = arg4*arg1 for arg3 items
C_PAUSE
      INTEGER*4 I
      DO I=1,N
        RB(I)=FAC*RA(I)
      ENDDO      
      RETURN
      END

      SUBROUTINE MVDA (RA,RB,N, FAC)
C_TITL  MVD  REAL*8 array add with a factor
      IMPLICIT NONE
C_ARGS
      REAL*8 RA(N)              ! [In]  Source array or constant
      REAL*8 RB(N)              ! [Out] Destination array
      INTEGER N                 ! [In]  Number of items to move:
      REAL*8 FAC                ! multiply factor
C_DESC  
C      arg2 = arg2 + arg4*arg1 for arg3 items
C_END
      INTEGER*4 I
      DO I=1,N
        RB(I)=RB(I) + FAC*RA(I)
      ENDDO      
      RETURN
      END

      SUBROUTINE MV21D (RA,ID1,RB,N)
C_TITL  MVD  REAL*8 2-dimen to REAL*8 1-dim array move
      IMPLICIT NONE
C_ARGS
      INTEGER ID1               ! [In]  First dimension of RA
      REAL*8 RA(ID1,N)          ! [In]  Source array
      REAL*8 RB(N)              ! [Out] Destination array
      INTEGER N                 ! [In]  Number of items to move:
C_USE
C If RA is (5,7) and you want all the (3,*) items, call as (RA(3,1),5, RB,7)
C " If you want (3,4:7) items  call as (RA(3,4),5, RB,4); etc.
C If you want consecutive items in Dimen 1, easier to use, e.g.,  MVD (RA(1,3),RB,5))
C_PAUSE
      INTEGER*4 I
      DO I=1,N
        RB(I)=RA(1,I)
      ENDDO      
      RETURN
      END
