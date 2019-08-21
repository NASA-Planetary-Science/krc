      REAL*4 FUNCTION AVERAG (AA,N)
C_Title  AVERAG  Get average of a real array
      IMPLICIT NONE
C_Args
      REAL*4 AA(*)                !in. array to be summed
      INTEGER*4 N                 !in. number of items to sum
C_Desc simple do-loop sum
C_Call  0
C_Hist  2008jan30  Hugh Kieffer  ORIGINAL_VERSION
C_End
      INTEGER I
      REAL*4 SUM

      SUM=0. 
      DO I=1,N
         SUM = SUM + AA(I)
      ENDDO
      AVERAG = SUM/FLOAT(N)
      RETURN
      END
