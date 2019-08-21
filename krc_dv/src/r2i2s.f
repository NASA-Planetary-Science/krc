      SUBROUTINE R2I2S(AA,N1,FOFF,FMUL, II)
C_TITL  R2I2S  SCALE  REAL INTO INT2 VECTOR I=floor((R-FOFF)*FMUL)
      INTEGER N1            !in. dimensions of AA
      REAL*4 AA(N1)             !in. 
      REAL*4 FOFF               !in. offset to subtract from AA
      REAL*4 FMUL               !in.  multipling factor
      INTEGER*2 II(N1)          !out. Scaled Int2 of input
C_Hist 2011aug11 Hugh Kieffer
  
      INTEGER J
      REAL*4 Y

      DO J=1,N1
         Y=(AA(J)-FOFF)*FMUL    ! scale onto +/- 2^15
         II(J)=INT2(FLOOR(Y))   ! truncate and convert
      ENDDO

      RETURN
      END
