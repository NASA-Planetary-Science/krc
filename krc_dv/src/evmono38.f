      SUBROUTINE EVMONO38 (CC,M,XX,XOFF,XMUL, YY)
C_Titl  EVMONO3  Evaluate monomial of 3rd degree for a vector input, with scaling
      IMPLICIT NONE
      REAL*8 CC(4)              ! in.  Coefficents
      INTEGER M                 ! in.  Number of independent values
      REAL*8 XX(M)              ! in.  Independent values
      REAL*8 XOFF               ! in.  Value to subtract from all  XX
      REAL*8 XMUL               ! in.  Multiply factor for offset  XX
      REAL*8 YY(M)              ! out. Dependant values
C_Hist 2008nov04  Hugh  Kieffer: for thermal conductivity
C 2010jan09 HK  Replace do loop for powers with explicit code 
C 2012feb26 HK  Remove unused variables
C 2014mar10 HK Make  REAL*8  version  
C_End
      INTEGER J
      REAL*8 X

      DO J=1,M                  ! each input value
         X=(XX(J)-XOFF)*XMUL
         YY(J)=(((CC(4)*X)+CC(3))*X+CC(2))*X+CC(1)
      ENDDO

      RETURN
      END
