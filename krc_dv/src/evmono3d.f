      SUBROUTINE EVMONO3D (CC,M,XX, YY)
C_Titl  EVMONO3D  Evaluate scaled monomial of 3rd degree for a vector input
      IMPLICIT NONE
      REAL*8 CC(4)              ! in.  Coefficents
      INTEGER M                 ! in.  Number of independent values
      REAL*8 XX(M)              ! in.  Independent values
      REAL*8 YY(M)              ! out. Dependant values

C_Hist 2017mar14  Hugh  Kieffer: for thermal conductivity. Adopt from evmono38.f
C       Make the scaling firm-code to speed up by transfering 2 fewer arguments
C 2017oct05 HK Correct value of  XOFF  from 200
C_End
      REAL*8 XOFF /220.d0/              ! Value to subtract from all  XX
      REAL*8 XMUL /1.D-2 /              ! Multiply factor for offset  XX
      INTEGER J
      REAL*8 X

      DO J=1,M                  ! each input value
         X=(XX(J)-XOFF)*XMUL ! scale the input to order unity
         YY(J)=(((CC(4)*X)+CC(3))*X+CC(2))*X+CC(1) ! 3rd degree polynomial
      ENDDO

      RETURN
      END
